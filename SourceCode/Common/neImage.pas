unit neImage;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Objects,
  FMX.Graphics, neImageList;

type
  TOnGetBitmap = procedure(aBitmap: TBitmap) of object;

  [ComponentPlatforms (pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or
                  pidAndroid or pidAndroid64 or pidiOSDevice or pidiOSSimulator
                      or pidiOSDevice32 or pidiOSDevice64)]
  TneImage = class(TImage)
  private
    fBaseScale: single;
    fDelimiterString: string;
    fDirectory: string;
    fFileExtension: string;
    fImageTag: string;
    fOnGetBitmap: TOnGetBitmap;
    fImageList: TneImageList; //PALOFF
    fVersion: string;
    procedure loadImage;
    procedure setBaseScale(const Value: single);
    procedure setDelimiterString(const Value: string);
    procedure setDirectory(const Value: string);
    procedure setFileExtension(const Value: string);
    procedure setImageTag(const Value: string);

    procedure OnLoadImages(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BaseScale: single read fBaseScale write setBaseScale;  //PALOFF
    property DelimiterString: string read fDelimiterString write setDelimiterString; //PALOFF
    property Directory: string read fDirectory write setDirectory;  //PALOFF
    property ImageTag: string read fImageTag write setImageTag;  //PALOFF
    property FileExtension: string read fFileExtension write setFileExtension; //PALOFF
    property Version: string read fVersion;     //PALOFF

    property OnGetBitmap: TOnGetBitmap read fOnGetBitmap write fOnGetBitmap; //PALOFF
  end;

procedure Register; //PALOFF

implementation

uses
  neImageTools.Types;

{$I VersionInfo.inc}

procedure Register;
begin
  RegisterComponents('NusEnvision', [TneImage]);
end;

constructor TneImage.Create(AOwner: TComponent);
begin
  inherited;
  fImageList:=TneImageList.Create(self);
  fDelimiterString:='@';
  fBaseScale:=neImageTools.Types.defaultScale;
  fVersion:=MajorVersion+'.'+MinorVersion+'.'+BugVersion;
  fFileExtension:='png';
end;

procedure TneImage.loadImage;
begin
  if Trim(fImageTag)='' then
    Self.Bitmap:=nil
  else
  begin
    TThread.Queue(nil,
      procedure
      begin
        fImageList.BeginUpdate;
        try
          fImageList.OnLoadImages:=OnLoadImages;
          fImageList.Directory:=fDirectory;
          fImageList.BaseScale:=fBaseScale;
          fImageList.FileExtension:=fFileExtension;
          fImageList.FilterTag:=fImageTag;
          fImageList.DelimiterString:=fDelimiterString;
        finally
          fImageList.EndUpdate;
        end;
      end);
  end;
end;

procedure TneImage.OnLoadImages(Sender: TObject);
begin
  if fImageList.BitmapExists(0) then
    if (Trim(fImageTag)<>'') then
      self.Bitmap:=fImageList.Bitmap(Self.Size.Size, 0)
    else
      Self.Bitmap:=nil
  else
    Self.Bitmap:=nil;
end;

{ TneImage }

procedure TneImage.Paint;
begin
  if Assigned(fOnGetBitmap) then
    fOnGetBitmap(Self.Bitmap);
  inherited;
end;

procedure TneImage.setBaseScale(const Value: single);
begin
  if fBaseScale<>Value then
  begin
    if Value=0 then
      fBaseScale:=neImageTools.Types.defaultScale
    else    //PALOFF
      fBaseScale := Value;
    loadImage;
  end;
end;

procedure TneImage.setDelimiterString(const Value: string);
begin
  if fDelimiterString<>Value then
  begin
    fDelimiterString := Value;
    loadImage;
  end;
end;

procedure TneImage.setDirectory(const Value: string);
begin
  if fDirectory<>Value then
  begin
    fDirectory := Value;
    loadImage;
  end;
end;

procedure TneImage.setFileExtension(const Value: string);
begin
  if fFileExtension<>Value then
  begin
    fFileExtension := Value;
    loadImage;
  end;
end;

procedure TneImage.setImageTag(const Value: string);
begin
  if fImageTag<>Value then
  begin
    fImageTag := Value;
    loadImage;
  end;
end;

end.
