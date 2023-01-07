unit UFiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

function FindRelevantFiles(const BasePath: String; const ExcludedPaths: TStrings): TStrings;

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
        if Result.Strings[Index].Remove(0, BasePath.Length).ToLower.StartsWith(LoweredExcludedPath) then
          Result.Delete(Index);
    end;
end;

end.
