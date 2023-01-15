unit UEncoding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UInputFiles;

procedure EncodeFilesChunks(const FilesChunks: TFilesChunks; const TargetStream: TStream);

implementation

uses
  SysUtils,
  Math,
  Contnrs,
  UEncoder;

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
    procedure GetNextFilesChunk(out FilesChunkId: ValSInt; out FilesChunk: TFileInfoArray);
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

procedure TFilesChunksManager.GetNextFilesChunk(out FilesChunkId: ValSInt; out FilesChunk: TFileInfoArray);
begin
  EnterCriticalSection(FCriticalSection);
  if FFilesChunkIndex < Length(FFilesChunks) then
  begin
    FilesChunkId := FFilesChunkIndex;
    FilesChunk := FFilesChunks[FFilesChunkIndex];
    Inc(FFilesChunkIndex);
  end
  else
  begin
    FilesChunkId := -1;
    FilesChunk := nil;
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

procedure EncodeFilesChunk(const FilesChunkId: UInt16; const FilesChunk: TFileInfoArray; const TargetStream: TStream);
var
  InputStream: TStream;
  OutputStream: TStream;
  FileInfo: TFileInfo;
  FileStream: TStream;
begin
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
    OutputStream := TMemoryStream.Create;
    UEncoder.Encode(InputStream, OutputStream);

    // Write ID of this chunk.
    TargetStream.WriteWord(FilesChunkId);

    // Write size of encoded data.
    TargetStream.WriteQWord(OutputStream.Size);

    // Copy encoded stream contents over.
    TargetStream.CopyFrom(OutputStream, 0);
  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;

type
  // The encoder thread gathers file chunks from the file chunk manager and writes the encoded result into the output writer thread.
  TEncoderThread = class(TThread)
  private
    FChunksManager: TFilesChunksManager;
    FOutputWriterThread: TOutputWriterThread;
  protected
    procedure Execute; override;
  public
    constructor Create(const ChunksManager: TFilesChunksManager; const OutputWriterThread: TOutputWriterThread);
  end;

constructor TEncoderThread.Create(const ChunksManager: TFilesChunksManager; const OutputWriterThread: TOutputWriterThread);
begin
  inherited Create(True);
  FChunksManager := ChunksManager;
  FOutputWriterThread := OutputWriterThread;
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
    FChunksManager.GetNextFilesChunk(FilesChunkId, FilesChunk);
    if (FilesChunkId < 0) or not Assigned(FilesChunk) then
    begin
      Self.Terminate;
      Break;
    end;

    Stream := TMemoryStream.Create;
    EncodeFilesChunk(FilesChunkId, FilesChunk, Stream);
    FOutputWriterThread.AddDataToWrite(Stream);
  end;
end;

procedure EncodeFilesChunks(const FilesChunks: TFilesChunks; const TargetStream: TStream);
var
  FilesChunksManager: TFilesChunksManager;
  OutputWriter: TOutputWriterThread;
  Encoders: array of TEncoderThread;
  Index: ValSInt;
  EncodersFinished: Boolean;
begin
  // Prepare all threads and file chunks.
  FilesChunksManager := TFilesChunksManager.Create(FilesChunks);

  OutputWriter := TOutputWriterThread.Create(TargetStream);
  OutputWriter.Start;

  Encoders := nil;
  SetLength(Encoders, Math.Min(GetCPUCount, Length(FilesChunks)));
  for Index := 0 to High(Encoders) do
  begin
    Encoders[Index] := TEncoderThread.Create(FilesChunksManager, OutputWriter);
    // Let the games begin!
    Encoders[Index].Start;
  end;

  // Wait for all encoder threads to finish work by polling every now and then.
  repeat
    EncodersFinished := True;
    for Index := 0 to High(Encoders) do
      EncodersFinished := EncodersFinished and Encoders[Index].Finished;
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
