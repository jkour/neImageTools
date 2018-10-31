unit neImageList.Utils;

interface

uses
  System.Generics.Collections, System.Types, FMX.ImgList, neImageList, System.Classes;

procedure processDirectories(const aDirectory, aFilterTag, aExtension: string; //PALOFF
    aList: TList<string>);

procedure processImages(const aDelimiter: string; const aList: TList<string>; //PALOFF
    fileDictionary: TDictionary<string, TStringDynArray>);

procedure processImageList(const aDictionary: TDictionary<string,   //PALOFF
    TStringDynArray>; const aScale: single; aPointerList: Pointer;
      aOnLoad: TNotifyEvent);

implementation

uses
  System.Threading, System.SysUtils, System.IOUtils, System.StrUtils, FMX.MultiResBitmap, FMX.Graphics, System.SyncObjs;

procedure processDirectories(const aDirectory, aFilterTag, aExtension: string;
    aList: TList<string>);
var
  dirList: TStringList;
  dirTask: ITask;
  outList: TList<string>;
begin

  outList:=aList;

  outList.Clear;

  if Trim(aDirectory)='' then
    Exit;

  dirList:=TStringList.Create;
  try
    dirList.Delimiter:=';';
    dirList.StrictDelimiter:=True;
    dirList.DelimitedText:=aDirectory;

    dirTask:=TTask.Run(
      procedure
      var
        dir: string;
        sr: TSearchRec;
        path: string;
      begin
        for dir in dirList do
        begin
          if DirectoryExists(trim(dir)) then
          begin
            path:=TPath.Combine(
                        trim(dir), trim(aFilterTag+'*.'+trim(aExtension)));
            if FindFirst(path, 0, sr) = 0  then
            begin
              repeat
                outList.Add(TPath.Combine(dir, sr.Name));
              until FindNext(sr) <> 0;
              FindClose(sr);
            end;
          end;
        end;
      end);

    TTask.WaitForAll(dirTask);

  finally
    dirList.Free;
  end;

end;

procedure processImages(const aDelimiter: string; const aList: TList<string>;
    fileDictionary: TDictionary<string, TStringDynArray>);
var
  dict: TDictionary<string, TStringDynArray>;
  task: ITask;
begin
  dict:=fileDictionary;
  dict.Clear;
  task:=TTask.Run(
    procedure
    var
      fullDir: string;
      filename: string;
      pathStrArray,
      fileStrArray: TStringDynArray;
    begin
      for fulldir in aList do
      begin
        filename:=ExtractFileName(fulldir);
        if Pos(aDelimiter, filename, Low(filename)) > 0 then
        begin
          SetLength(pathStrArray, 0);
          pathStrArray:=SplitString(filename, aDelimiter);
          if Length(pathStrArray)>1 then
          begin
            if dict.ContainsKey(pathStrArray[0]) then
              fileStrArray:=dict.Items[pathStrArray[0]]
            else
              SetLength(fileStrArray, 0);
            SetLength(fileStrArray, Length(fileStrArray) + 1);
            fileStrArray[Length(fileStrArray) - 1]:= fulldir;
            dict.AddOrSetValue(pathStrArray[0], fileStrArray);
          end;
        end;
      end;
    end);
  TTask.WaitForAll([task]);
end;

procedure processImageList(const aDictionary: TDictionary<string,
    TStringDynArray>; const aScale: single; aPointerList: Pointer;
            aOnLoad: TNotifyEvent);
var
  aList: TneImageList;
  scale: Single;
  imageName,
  filename: string;
  bitmap: TBitmap;

  scales: array of Single;
  filenames: array of string;

begin
  aList:=TneImageList(aPointerList^);

  aList.Source.Clear;
  aList.Destination.Clear;

  for imageName in aDictionary.Keys do
  begin
    SetLength(scales, 0);
    SetLength(filenames, 0);
    for filename in aDictionary.Items[imageName] do
    begin
      if FileExists(filename) then
      begin
        bitmap:=TBitmap.Create;
        try
          try
            bitmap.LoadFromFile(filename);

            scale := bitmap.Width / aScale;

            SetLength(scales, Length(scales)+1);
            scales[Length(scales) - 1]:=scale;

            SetLength(filenames, Length(filenames)+1);
            filenames[Length(filenames) - 1]:=filename;
          except
            ; //FI: W501
          end;
        finally
          bitmap.Free;
        end;
      end;
    end;
    aList.AddOrSet(imageName, scales, filenames);
  end;

  if Assigned(aOnLoad) then
    aOnLoad(nil);

end;

end.
