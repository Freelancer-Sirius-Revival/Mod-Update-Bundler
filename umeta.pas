unit UMeta;

{$mode ObjFPC}{$H+}

interface

const
  MetaFileExtension = '.meta';

procedure CreateMetaFileForFile(const FileName: String);

implementation

uses
  Classes,
  SysUtils,
  UBundle,
  md5;

procedure CreateMetaFileForFile(const FileName: String);
var
  BundleStream: TFileStream;
  BundleFileSize: Int64;
  ContentVersion: Uint32;
  BundleType: TBundleType;
  FileEntries: TFileEntries;
  FileEntry: TFileEntry;
  AllFilesSize: Int64;
  MetaStream: TMemoryStream;
begin
  if FileExists(FileName) then
  begin
    try
      try
        BundleStream := TFileStream.Create(FileName, fmOpenRead);
        BundleFileSize := BundleStream.Size;
        BundleStream.Position := 0;
        ContentVersion := ReadContentVersion(BundleStream);
        BundleStream.Position := 0;
        BundleType := ReadBundleType(BundleStream);
        BundleStream.Position := 0;
        FileEntries := ReadFilesMetaData(BundleStream);
      except
        Exit;
      end;
    finally
      BundleStream.Free;
    end;

    AllFilesSize := 0;
    for FileEntry in FileEntries do
      AllFilesSize := AllFilesSize + FileEntry.Size;

    try
      MetaStream := TMemoryStream.Create;
      MetaStream.WriteByte(Ord(BundleType));
      MetaStream.WriteDWord(ContentVersion);
      MetaStream.WriteQWord(BundleFileSize);
      MetaStream.WriteQWord(AllFilesSize);
      MetaStream.WriteDWord(Length(FileEntries));
      for FileEntry in FileEntries do
      begin
        MetaStream.WriteAnsiString(FileEntry.Path);
        MetaStream.Write(FileEntry.Checksum, SizeOf(TMD5Digest));
      end;
      MetaStream.SaveToFile(FileName + MetaFileExtension);
    finally
      MetaStream.Free;
    end;
  end;
end;

end.
