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

    try
      MetaStream := TMemoryStream.Create;
      // Version of file contents.
      MetaStream.WriteDWord(ContentVersion);
      // Bundle type.
      MetaStream.WriteByte(Ord(BundleType));
      // Total size of the compressed bundle.
      MetaStream.WriteQWord(BundleFileSize);
      // Count of files in this bundle.
      MetaStream.WriteDWord(Length(FileEntries));

      for FileEntry in FileEntries do
      begin                       
        // First write a normalized relative path of the file.
        MetaStream.WriteAnsiString(FileEntry.Path);
        // Second write the uncompressed size of the file.
        MetaStream.WriteQWord(FileEntry.Size);
        // Third write an MD5 hash of the file's contents.
        MetaStream.Write(FileEntry.Checksum, SizeOf(TMD5Digest));
      end;
      MetaStream.SaveToFile(FileName + MetaFileExtension);
    finally
      MetaStream.Free;
    end;
  end;
end;

end.
