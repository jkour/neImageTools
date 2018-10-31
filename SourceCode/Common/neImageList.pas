unit neImageList;

interface

uses
  System.Classes, System.ImageList, FMX.ImgList, System.Generics.Collections,
  System.Types;

type

  [ComponentPlatforms(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or
    pidAndroid or pidAndroid64 or pidiOSDevice or pidiOSSimulator or
    pidiOSDevice32 or pidiOSDevice64)]
  TneImageList = class(TImageList)
  private
    fChainingCalls: Boolean;
    fOnLoadImages: TNotifyEvent;
    fVersion: string;
    fBaseScale: single;
    fDirectory: string;
    fFileExtension: string;
    fDelimiterString: string;
    fFilterTag: string;
    fFilenames: TList<string>;
    fDictionary: TDictionary<string, TStringDynArray>;

    procedure setBaseScale(const Value: single);
    procedure setDelimiterString(const Value: string);
    procedure setDirectory(const Value: string);
    procedure setFileExtension(const Value: string);
    procedure setFilterTag(const Value: string);
  protected
    procedure loadImages;
    procedure loadDirectories; overload;  //PALOFF
    procedure createImageList; overload;  //PALOFF
    procedure loadImageList;        //PALOFF
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;   //PALOFF
    procedure EndUpdate;     //PALOFF
  published
    property BaseScale: single read fBaseScale write setBaseScale;  //PALOFF
    property DelimiterString: string read fDelimiterString   //PALOFF
      write setDelimiterString;
    property Directory: string read fDirectory write setDirectory; //PALOFF
    property FileExtension: string read fFileExtension write setFileExtension;  //PALOFF
    property FilterTag: string read fFilterTag write setFilterTag;   //PALOFF
    property Version: string read fVersion;                     //PALOFF

    property OnLoadImages: TNotifyEvent read fOnLoadImages write fOnLoadImages; //PALOFF
  end;

procedure Register;  //PALOFF

implementation

uses
  System.SysUtils, FMX.Dialogs, System.Threading, System.IOUtils,
  System.StrUtils, neImageList.Utils, FMX.Graphics, FMX.MultiResBitmap,
  neImageTools.Types;

{$I VersionInfo.inc}

procedure Register;
begin
  RegisterComponents('NusEnvision', [TneImageList]);
end;

procedure TneImageList.BeginUpdate;
begin
  fChainingCalls := True;
end;

constructor TneImageList.Create(AOwner: TComponent);
begin
  inherited;
  fBaseScale := neImageTools.Types.defaultScale;
  fFilenames := TList<string>.Create;
  fDelimiterString := '@';
  fFileExtension := 'png';
  fDictionary := TDictionary<string, TStringDynArray>.Create;
  fVersion := MajorVersion + '.' + MinorVersion + '.' + BugVersion;
end;

destructor TneImageList.Destroy;
begin
  fFilenames.Free;
  fDictionary.Free;
  inherited;
end;

procedure TneImageList.EndUpdate;
begin
  fChainingCalls := False;
  loadImages;
end;

procedure TneImageList.loadDirectories;
begin
  processDirectories(fDirectory, fFilterTag, fFileExtension, fFilenames);
end;

procedure TneImageList.loadImageList;
begin
  processImageList(fDictionary, fBaseScale, @self, fOnLoadImages);
end;

procedure TneImageList.loadImages;
begin
  loadDirectories;
  createImageList;
  loadImageList;
end;

procedure TneImageList.createImageList;
begin
  processImages(fDelimiterString, fFilenames, fDictionary);
end;

procedure TneImageList.setBaseScale(const Value: single);
begin
  if fBaseScale <> Value then
  begin
    if Value = 0 then
      fBaseScale := neImageTools.Types.defaultScale
    else       //PALOFF
      fBaseScale := Value;
    if not fChainingCalls then
      loadImages;
  end;
end;

procedure TneImageList.setDelimiterString(const Value: string);
begin
  if fDelimiterString <> Value then
  begin
    fDelimiterString := Value;
    if not fChainingCalls then
      loadImages;
  end;
end;

procedure TneImageList.setDirectory(const Value: string);
begin
  if fDirectory <> Value then
  begin
    fDirectory := Value;
    if not fChainingCalls then
      loadImages;
  end;
end;

procedure TneImageList.setFileExtension(const Value: string);
begin
  if fFileExtension <> Value then
  begin
    fFileExtension := Value;
    if not fChainingCalls then
      loadImages;
  end;
end;

procedure TneImageList.setFilterTag(const Value: string);
begin
  if fFilterTag <> Value then
  begin
    fFilterTag := Value;
    if not fChainingCalls then
      loadImages;
  end;
end;

end.
