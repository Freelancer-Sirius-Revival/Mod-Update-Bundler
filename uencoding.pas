unit UEncoding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UInputFiles;

type
  TEncodingProgressCallback = procedure(const BytesWritten: Int64) of object;

procedure EncodeFilesChunks(const FilesChunks: TFilesChunks; const TargetStream: TStream; const OnEncodingProgress: TEncodingProgressCallback);

implementation

uses
  SysUtils,
  Math,
  Contnrs,
  UEncoder,
  {$IFDEF UNIX}
  UTF8Process,
  {$ENDIF}
  ULZMACommon;

type
  // This class contains all file chunks that will be processed by the bundler threads.
  TFilesChunksManager = class
  private
    FFilesChunks: TFilesChunks;
    FFilesChunkIndex: ValSInt;
    FCriticalSection: TRTLCriticalSection;
  public
    constructor Create(const FilesChunks: TFilesChunks);
    destructor Destroy; override;
    function GetNextFilesChunk(out FilesChunkId: ValSInt; out FilesChunk: TFileInfoArray): Boolean;
  end;

constructor TFilesChunksManager.Create(const FilesChunks: TFilesChunks);
begin
  inherited Create;
  FFilesChunks := FilesChunks;
  FFilesChunkIndex := 0;
  InitCriticalSection(FCriticalSection);
end;

destructor TFilesChunksManager.Destroy;
begin
  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function TFilesChunksManager.GetNextFilesChunk(out FilesChunkId: ValSInt; out FilesChunk: TFileInfoArray): Boolean;
begin
  Result := False;
  FilesChunkId := -1;
  FilesChunk := nil;

  EnterCriticalSection(FCriticalSection);
  if FFilesChunkIndex < Length(FFilesChunks) then
  begin
    FilesChunkId := FFilesChunkIndex;
    FilesChunk := FFilesChunks[FFilesChunkIndex];
    Inc(FFilesChunkIndex);
    Result := True;
  end;
  LeaveCriticalSection(FCriticalSection);
end;

type
  // The output writer thread gathers each stream that was finished by the bundler threads and writes it into the destination stream.
  // Due the nature of multi threading, the order of streams written into the destination may vary each time.
  TOutputWriterThread = class(TThread)
  private
    FDestinationStream: TStream;
    FCriticalSection: TRTLCriticalSection;
    FDataStack: TObjectStack;
  protected
    procedure Execute; override;
  public
    constructor Create(const DestinationStream: TStream);
    destructor Destroy; override;
    procedure AddDataToWrite(const Stream: TStream);
    function GetUnprocessedCount: Int64;
  end;

constructor TOutputWriterThread.Create(const DestinationStream: TStream);
begin
  inherited Create(True);
  FDestinationStream := DestinationStream;
  InitCriticalSection(FCriticalSection);
  FDataStack := TObjectStack.Create;
end;

destructor TOutputWriterThread.Destroy;
begin
  DoneCriticalSection(FCriticalSection);
  while FDataStack.Count > 0 do
    FDataStack.Pop.Free;
  FDataStack.Free;
  inherited Destroy;
end;

procedure TOutputWriterThread.AddDataToWrite(const Stream: TStream);
begin
  EnterCriticalSection(FCriticalSection);
  FDataStack.Push(Stream);
  LeaveCriticalSection(FCriticalSection);
end;

function TOutputWriterThread.GetUnprocessedCount: Int64;
begin
  EnterCriticalSection(FCriticalSection);
  Result := FDataStack.Count;
  LeaveCriticalSection(FCriticalSection);
end;

procedure TOutputWriterThread.Execute;
var
  StreamToWrite: TStream = nil;
begin
  if not Assigned(FDestinationStream) then
    Self.Terminate;

  while not Self.Terminated do
  begin
    EnterCriticalSection(FCriticalSection);
    if FDataStack.Count > 0 then
      StreamToWrite := TStream(FDataStack.Pop);
    LeaveCriticalSection(FCriticalSection);

    if Assigned(StreamToWrite) then
    begin
      FDestinationStream.CopyFrom(StreamToWrite, 0);
      FreeAndNil(StreamToWrite);
    end;

    Sleep(50);
  end;
end;

function EncodeFilesChunk(const FilesChunkId: Uint16; const FilesChunk: TFileInfoArray; const TargetStream: TStream; const OnProgress: TLZMAProgress): Boolean;
var
  InputStream: TStream;
  EncodedStream: TStream;
  FileInfo: TFileInfo;
  FileStream: TStream;
begin
  Result := False;

  // Read all file contents of this chunk.
  InputStream := TMemoryStream.Create;
  for FileInfo in FilesChunk do
  begin
    try
      FileStream := TFileStream.Create(FileInfo.Path, fmOpenRead, fmShareDenyWrite);
      InputStream.CopyFrom(FileStream, 0);
    finally
      FileStream.Free;
    end;
  end;

  try
    // Encode all file contents.
    InputStream.Position := 0;
    EncodedStream := TMemoryStream.Create;
    if not Encode(InputStream, EncodedStream, OnProgress) then
      Exit;

    // Write ID of this chunk.
    TargetStream.WriteWord(FilesChunkId);

    // Write size of encoded data.
    TargetStream.WriteQWord(EncodedStream.Size);

    // Copy encoded stream contents over.
    TargetStream.CopyFrom(EncodedStream, 0);
    Result := True;
  finally
    InputStream.Free;
    EncodedStream.Free;
  end;
end;

type
  // The encoder thread gathers file chunks from the file chunk manager and writes the encoded result into the output writer thread.
  TEncoderThread = class(TThread)
  private
    FChunksManager: TFilesChunksManager;
    FOutputWriterThread: TOutputWriterThread;
    FPreviousProgressData: Int64;
    FTotalBytesWritten: Int64;
    procedure OnEncoderProcess(const Action: TLZMAProgressAction; const Value: Int64);
  protected
    procedure Execute; override;
  public
    property TotalBytesWritten: Int64 read FTotalBytesWritten;
    constructor Create(const ChunksManager: TFilesChunksManager; const OutputWriterThread: TOutputWriterThread);
  end;

constructor TEncoderThread.Create(const ChunksManager: TFilesChunksManager; const OutputWriterThread: TOutputWriterThread);
begin
  inherited Create(True);
  FChunksManager := ChunksManager;
  FOutputWriterThread := OutputWriterThread;
  FTotalBytesWritten := 0;
end;

procedure TEncoderThread.OnEncoderProcess(const Action: TLZMAProgressAction; const Value: Int64);
begin
  if Action = LPAPos then
  begin
    InterlockedExchangeAdd64(FTotalBytesWritten, Value - FPreviousProgressData);
    FPreviousProgressData := Value;
  end;
end;

procedure TEncoderThread.Execute;
var
  FilesChunkId: ValSInt;
  FilesChunk: TFileInfoArray;
  Stream: TStream;
begin
  if not Assigned(FChunksManager) or not Assigned(FOutputWriterThread) then
    Self.Terminate;

  while not Self.Terminated do
  begin
    if not FChunksManager.GetNextFilesChunk(FilesChunkId, FilesChunk) then
    begin
      Self.Terminate;
      Break;
    end;

    FPreviousProgressData := 0;
    Stream := TMemoryStream.Create;
    if EncodeFilesChunk(FilesChunkId, FilesChunk, Stream, @OnEncoderProcess) then
      FOutputWriterThread.AddDataToWrite(Stream);
  end;
end;

procedure EncodeFilesChunks(const FilesChunks: TFilesChunks; const TargetStream: TStream; const OnEncodingProgress: TEncodingProgressCallback);
var
  FilesChunksManager: TFilesChunksManager;
  OutputWriter: TOutputWriterThread;
  Encoders: array of TEncoderThread;
  Index: ValSInt;
  EncodersFinished: Boolean;
  TotalBytesWritten: Int64;
begin
  // Prepare all threads and file chunks.
  FilesChunksManager := TFilesChunksManager.Create(FilesChunks);

  if Assigned(OnEncodingProgress) then
    OnEncodingProgress(0);

  OutputWriter := TOutputWriterThread.Create(TargetStream);
  OutputWriter.Start;

  Encoders := nil;
  SetLength(Encoders, Math.Min({$IFDEF UNIX}GetSystemThreadCount{$ELSE}GetCPUCount{$ENDIF}, Length(FilesChunks)));
  for Index := 0 to High(Encoders) do
  begin
    Encoders[Index] := TEncoderThread.Create(FilesChunksManager, OutputWriter);
    // Let the games begin!
    Encoders[Index].Start;
  end;

  // Wait for all encoder threads to finish work by polling every now and then.
  repeat
    TotalBytesWritten := 0;
    EncodersFinished := True;
    for Index := 0 to High(Encoders) do
    begin
      TotalBytesWritten := TotalBytesWritten + Encoders[Index].TotalBytesWritten;
      EncodersFinished := EncodersFinished and Encoders[Index].Finished;
    end;
    
    if Assigned(OnEncodingProgress) then
      OnEncodingProgress(TotalBytesWritten);

    Sleep(50);
  until EncodersFinished;

  for Index := 0 to High(Encoders) do
  begin
    Encoders[Index].WaitFor;
    Encoders[Index].Free;
  end;

  // Wait for the output writer thread to have written every remaining piece of data before shutting it down.
  while OutputWriter.GetUnprocessedCount > 0 do
    Sleep(50);
  OutputWriter.Terminate;
  OutputWriter.WaitFor;
  OutputWriter.Free;

  FilesChunksManager.Free;
end;

end.
