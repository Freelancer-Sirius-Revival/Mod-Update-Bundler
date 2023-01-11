unit UBundle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UInputFiles,
  md5;

const
  FlsrFileVersion = 1;
  FlsrFileMagicNumbers = 'FLSR';
  FullBundleFileName = 'main';
  UpdateBundleFileName = 'update';
  BundleFileExtension = '.flsr';
  ChecksumFileExtension = '.md5';

type
  TFileEntry = record
    Path: String;
    Size: Int64;
    Checksum: TMD5Digest;
  end;
  TFileEntries = array of TFileEntry;

  TBundleType = (TFullBundle = 0, TUpdateBundle = 1, TUnknownBundle = 255);

procedure WriteMetaData(const ContentVersion: Uint32; const BundleType: TBundleType; const FilesChunks: TFilesChunks; const BasePath: String; const Stream: TStream);
function ReadFilesMetaData(const Stream: TStream): TFileEntries;
function ReadContentVersion(const Stream: TStream): Uint32;
function ReadBundleType(const Stream: TStream): TBundleType;

implementation

uses
  SysUtils;

procedure WriteMetaData(const ContentVersion: Uint32; const BundleType: TBundleType; const FilesChunks: TFilesChunks; const BasePath: String; const Stream: TStream);
var
  FilesChunk: TFileInfoArray;
  ChunkFile: TFileInfo;
begin
  // Magic number of file format.
  Stream.Write(FlsrFileMagicNumbers, SizeOf(FlsrFileMagicNumbers));

  // Version of file format.
  Stream.WriteByte(FlsrFileVersion);

  // Version of file contents.
  Stream.WriteDWord(ContentVersion);

  // Bundle type.
  Stream.WriteByte(Ord(BundleType));

  // Count of Chunks.
  Stream.WriteWord(Length(FilesChunks));

  for FilesChunk in FilesChunks do
  begin
    // Count of files in this chunk.
    Stream.WriteDWord(Length(FilesChunk));

    for ChunkFile in FilesChunk do
    begin
      // First write a normalized relative path of the file.
      Stream.WriteAnsiString(ChunkFile.Path.Remove(0, BasePath.Length).Trim.Replace('\', '/'));
      // Second write the uncompressed size of the file.
      Stream.WriteQWord(ChunkFile.Size);
      // Third write an MD5 hash of the file's contents.
      Stream.Write(MD5File(ChunkFile.Path), SizeOf(TMD5Digest));
    end;
  end;
end;

function ReadFilesMetaData(const Stream: TStream): TFileEntries;
var
  MagicNumbers: array [0..3] of Char;
  ChunksCount: ValSInt;
  ChunkIndex: ValSInt;
  FilesCount: ValSInt;
  ChunkFileIndex: ValSInt;
  FileNumber: ValSInt = 0;
begin
  Result := nil;

  // Magic number of file format.
  Stream.Read(MagicNumbers, SizeOf(FlsrFileMagicNumbers));
  if (Length(MagicNumbers) = Length(FlsrFileMagicNumbers)) and (CompareByte(MagicNumbers, FlsrFileMagicNumbers, SizeOf(MagicNumbers)) = 0) then
    Exit;

  // Version of file format.
  Stream.ReadByte;

  // Version of file contents.
  Stream.ReadDWord;

  // Bundle type.
  Stream.ReadByte;

  // Count of Chunks.
  ChunksCount := Stream.ReadWord;

  for ChunkIndex := 0 to ChunksCount - 1 do
  begin
    // Count of files in this chunk.
    FilesCount := Stream.ReadDWord;
    SetLength(Result, Length(Result) + FilesCount);

    for ChunkFileIndex := 0 to FilesCount - 1 do
    begin
      // First read a normalized relative path of the file.
      Result[FileNumber].Path := Stream.ReadAnsiString;
      // Second read the uncompressed size of the file.
      Result[FileNumber].Size := Stream.ReadQWord;
      // Third read an MD5 hash of the file's contents.
      Stream.Read(Result[FileNumber].Checksum, SizeOf(TMD5Digest));

      Inc(FileNumber);
    end;
  end;
end;

function ReadContentVersion(const Stream: TStream): Uint32;
var
  MagicNumbers: array [0..3] of Char;
begin
  Result := High(Uint32);

  // Magic number of file format.
  Stream.Read(MagicNumbers, SizeOf(FlsrFileMagicNumbers));
  if (Length(MagicNumbers) = Length(FlsrFileMagicNumbers)) and (CompareByte(MagicNumbers, FlsrFileMagicNumbers, SizeOf(MagicNumbers)) = 0) then
    Exit;

  // Version of file format.
  Stream.ReadByte;

  // Version of file contents.
  Result := Stream.ReadDWord;
end;

function ReadBundleType(const Stream: TStream): TBundleType;
var
  MagicNumbers: array [0..3] of Char;
begin
  Result := TUnknownBundle;

  // Magic number of file format.
  Stream.Read(MagicNumbers, SizeOf(FlsrFileMagicNumbers));
  if (Length(MagicNumbers) = Length(FlsrFileMagicNumbers)) and (CompareByte(MagicNumbers, FlsrFileMagicNumbers, SizeOf(MagicNumbers)) = 0) then
    Exit;

  // Version of file format.
  Stream.ReadByte;

  // Version of file contents.
  Stream.ReadDWord;

  // Bundle type.
  Result := TBundleType(Stream.ReadByte);
end;

end.
