unit Test.neImage.Main;

interface
uses
  DUnitX.TestFramework, neImage, FMX.Graphics;

type
  TTestneImage = class (TneImage)
  public
    procedure callPaint;
  end;

  [TestFixture]
  TneImageTest = class(TObject)
  private
    fneImage: TTestneImage;
    fOnGetTestString: string;
    procedure OnGetBitmap (aBitmap: TBitmap);

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure baseScaleAssignment;
    [Test]
    procedure delimiterAssignment;
    [Test]
    procedure fileExtensionAssignment;
    [Test]
    procedure ImageTagAssignment;

    [Test]
    procedure OnGetBitmapTest;
  end;

implementation

uses
  neImageList, neImageTools.Types;

procedure TneImageTest.baseScaleAssignment;
var
  current: single;
begin
  current:=fneImage.BaseScale;
  fneImage.BaseScale:=200;
  Assert.IsTrue(200 = fneImage.BaseScale,'BaseScale not retrieved correctly');
  fneImage.BaseScale:=0;
  Assert.IsTrue(defaultScale = fneImage.BaseScale, 'Default scale not set when 0');
  fneImage.BaseScale:=current;
end;

procedure TneImageTest.delimiterAssignment;
var
  current: string;
begin
  current:=fneImage.DelimiterString;
  fneImage.DelimiterString:='NewDelim';
  Assert.AreEqual('NewDelim',fneImage.DelimiterString,'DelimiterString not retrieved');
  fneImage.DelimiterString:=current;
end;

procedure TneImageTest.fileExtensionAssignment;
var
  current: string;
begin
  current:=fneImage.FileExtension;
  fneImage.FileExtension:='bat';
  Assert.AreEqual('bat',fneImage.FileExtension,'FileExtension not retrieved');
  fneImage.FileExtension:=current;
end;

procedure TneImageTest.ImageTagAssignment;
var
  current: string;
begin
  current:=fneImage.ImageTag;
  fneImage.ImageTag:='new-image';
  Assert.AreEqual('new-image',fneImage.ImageTag,'ImageTag not retrieved');
  fneImage.ImageTag:=current;
end;

procedure TneImageTest.OnGetBitmap(aBitmap: TBitmap);
begin
  fOnGetTestString:='OnGetImage';
end;

procedure TneImageTest.OnGetBitmapTest;
begin
  fOnGetTestString:='';
  fneImage.OnGetBitmap:=nil;
  fneImage.OnGetBitmap:=OnGetBitmap;
  fneImage.callPaint;
  Assert.AreEqual('OnGetImage', fOnGetTestString,
                            'Bitmap assignment does not work properly');
  fneImage.OnGetBitmap:=nil;
  fOnGetTestString:='';
end;

procedure TneImageTest.Setup;
begin
  fneImage:=TTestneImage.Create(nil);
end;

procedure TneImageTest.TearDown;
begin
  fneImage.Free;
end;

{ TTestneImage }

procedure TTestneImage.callPaint;
begin
  Self.Paint;
end;

initialization
  TDUnitX.RegisterTestFixture(TneImageTest);
end.
