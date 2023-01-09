unit UBundling;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UFiles;

procedure BundleFiles(const FilesChunks: TFilesChunks; const BasePath: String; const TargetStream: TStream);

implementation

uses
  SysUtils,
  UEncoding;

procedure BundleFilesChunk(const FilesChunk: TFileInfoArray; const BasePath: String; const TargetStream: TStream);
var
  InputStream: TStream;
  OutputStream: TStream;
  FileInfo: TFileInfo;
  FileStream: TStream;
  UncompressedChunkSize: Int64 = 0;
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

  // Encode all file contents.
  InputStream.Position := 0;
  OutputStream := TMemoryStream.Create;
  UEncoding.Encode(InputStream, OutputStream);
  InputStream.Free;

  // Write size of whole decompressed chunk.
  for FileInfo in FilesChunk do
    UncompressedChunkSize := UncompressedChunkSize + FileInfo.Size;         
  TargetStream.WriteQWord(UncompressedChunkSize);

  // Write count of files in this chunk.
  TargetStream.WriteQWord(Length(FilesChunk));

  // Write individual file path and size information.
  for FileInfo in FilesChunk do
  begin
    TargetStream.WriteAnsiString(FileInfo.Path.Remove(0, BasePath.Length).Trim.Replace('\', '/'));
    TargetStream.WriteQWord(FileInfo.Size);
  end;

  // Copy encoded stream contents over.
  TargetStream.CopyFrom(OutputStream, 0);
  OutputStream.Free;
end;

procedure BundleFiles(const FilesChunks: TFilesChunks; const BasePath: String; const TargetStream: TStream);
var
  FilesChunk: TFileInfoArray;
begin
  for FilesChunk in FilesChunks do
    BundleFilesChunk(FilesChunk, BasePath, TargetStream)
end;

end.

