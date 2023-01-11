unit UFiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type
  TFileInfo = record
    Path: String;
    Size: Int64;
  end;
  TFileInfoArray = array of TFileInfo;
  TFilesChunks = array of TFileInfoArray;

function FindRelevantFiles(const BasePath: String; const ExcludedPaths: TStrings): TStrings;
function ComputeChunkedFiles(const FileList: TStrings; const SplittingFileExtensions: array of String): TFilesChunks;

implementation

uses
  SysUtils,
  FileUtil;

function FindRelevantFiles(const BasePath: String; const ExcludedPaths: TStrings): TStrings;
var
  ExcludedPath: String;
  LoweredExcludedPath: String;
  Index: ValSInt;
begin
  Result := FindAllFiles(BasePath, AllFilesMask, True);
  if Assigned(ExcludedPaths) then
    for ExcludedPath in ExcludedPaths do
    begin
      // Normalize the entry from the excluded path list
      LoweredExcludedPath := ExcludedPath.Trim.ToLower.Replace('\', '/');
      if LoweredExcludedPath.IsEmpty then
        Continue;
      // Make sure the excluded path begins with a /
      if not LoweredExcludedPath.StartsWith('/') then
        LoweredExcludedPath := '/' + LoweredExcludedPath;

      for Index := Result.Count - 1 downto 0 do
        // Normalize the path and remove the base path part from it to compare it against the excluded path.
        if Result.Strings[Index].Remove(0, BasePath.Length).ToLower.Replace('\', '/').StartsWith(LoweredExcludedPath) then
          Result.Delete(Index);
    end;
end;

function GetFileSize(FileName: String): Int64;
var
  Info: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, Info) = 0 then
    Result := Info.Size
  else
    Result := -1;
  FindClose(Info);
end;

type
  TArrayOfTStrings = array of TStrings;

function SeparateFilesByExtension(const FileList: TStrings; const Extensions: array of String): TArrayOfTStrings;
var
  FilePath: String;
  LoweredFilePath: String;
  ExtensionIndex: ValSInt;
  ExtensionFoundInPath: Boolean;
begin
  Result := nil;
  SetLength(Result, Length(Extensions) + 1);
  for ExtensionIndex := 0 to High(Result) do
    Result[ExtensionIndex] := TStringList.Create;
  // Split files by Ini or non-ini (mostly UTF files and other binaries).
  for FilePath in FileList do
  begin
    LoweredFilePath := FilePath.ToLower;
    ExtensionFoundInPath := False;
    for ExtensionIndex := 0 to High(Extensions) do
      if LoweredFilePath.EndsWith(Extensions[ExtensionIndex].ToLower) then
      begin
        Result[ExtensionIndex].Append(FilePath);
        ExtensionFoundInPath := True;
        Break;
      end;

    if not ExtensionFoundInPath then
      Result[High(Result)].Append(FilePath);
  end;
end;

function GetSizeSortedFiles(const FilePaths: TStrings): TFileInfoArray;

  procedure InsertFileSize(const Path: String; const Size: Int64; var FileSizesCount: ValSInt);
  var
    Index: ValSInt;
    InsertIndex: ValSInt;
  begin
    InsertIndex := FileSizesCount; // Default with the new end index after counting up the count.
    for Index := 0 to FileSizesCount - 1 do
      if Result[Index].Size >= Size then
      begin
        InsertIndex := Index;
        Break;
      end;

    // Set the new count.
    Inc(FileSizesCount);

    // Move all elements greater than the InsertIndex by 1 towards the end.
    for Index := FileSizesCount - 1 downto InsertIndex + 1 do
      Result[Index] := Result[Index - 1];

    Result[InsertIndex].Path := Path;
    Result[InsertIndex].Size := Size;
  end;

var
  Index: ValSInt;
  FileSizesCount: ValSInt = 0;
begin
  Result := nil;
  SetLength(Result, FilePaths.Count);
  for Index := 0 to FilePaths.Count - 1 do
    InsertFileSize(FilePaths.Strings[Index], GetFileSize(FilePaths.Strings[Index]), FileSizesCount);
end;

function AssembleFilesChunk(const FileSizes: TFileInfoArray; const StartIndex: ValSInt; const MaxChunkSize: Uint32): TFileInfoArray;
var
  Index: ValSInt;
  AccumulatedSize: Uint32 = 0;
  CollectedFilesCount: ValSInt = 0;
begin
  Result := nil;
  SetLength(Result, Length(FileSizes) - StartIndex);
  for Index := StartIndex to High(FileSizes) do
    if AccumulatedSize + FileSizes[Index].Size <= MaxChunkSize then
    begin
      AccumulatedSize := AccumulatedSize + FileSizes[Index].Size;
      Result[CollectedFilesCount] := FileSizes[Index];
      Inc(CollectedFilesCount);
    end;

  // If any remaining file size is bigger than the actual chunk target size.
  if CollectedFilesCount = 0 then
  begin
    Result[0] := FileSizes[StartIndex];
    CollectedFilesCount := 1;
  end;

  SetLength(Result, CollectedFilesCount);
end;

function ComputeChunkedFiles(const FileList: TStrings; const SplittingFileExtensions: array of String): TFilesChunks;
var
  SeparatedFilePaths: TArrayOfTStrings;
  FilePaths: TStrings;
  SortedFileSizes: TFileInfoArray;
  SortedFileSizesIndex: ValSInt;
  CreatedChunkedFileSizes: TFileInfoArray;
  ChunkedFileSizesIndex: ValSInt = 0;
begin
  Result := nil;
  if not Assigned(FileList) then
    Exit;

  SetLength(Result, FileList.Count);

  // 1. separate all files by their extensions for more optimal compression results of similar files.
  SeparatedFilePaths := SeparateFilesByExtension(FileList, SplittingFileExtensions);
  for FilePaths in SeparatedFilePaths do
  begin
    // 2. Get all sizes for files and have them being sorted by that.
    SortedFileSizes := GetSizeSortedFiles(FilePaths);
    FilePaths.Free;

    // 3. Put these files into chunks.
    SortedFileSizesIndex := 0;
    while SortedFileSizesIndex < Length(SortedFileSizes) do
    begin
      CreatedChunkedFileSizes := AssembleFilesChunk(SortedFileSizes, SortedFileSizesIndex, 1 shl 27 {128MB});
      Result[ChunkedFileSizesIndex] := CreatedChunkedFileSizes;
      SortedFileSizesIndex := SortedFileSizesIndex + Length(CreatedChunkedFileSizes);
      Inc(ChunkedFileSizesIndex);
    end;
  end;
  SetLength(Result, ChunkedFileSizesIndex);
end;

end.
