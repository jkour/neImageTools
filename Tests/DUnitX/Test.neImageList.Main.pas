unit Test.neImageList.Main;

interface
uses
  DUnitX.TestFramework, neImageList, System.Classes, System.Types;

type

  [TestFixture]
  TneImageListTest = class(TObject)
  private
    fTestScale: Single;
    fneImageList: TneImageList;

    procedure OnLoadImages(Sender: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure baseScaleAssignment;
    [Test]
    procedure delimiterAssignment;
    [Test]
    procedure directoryAssignment;
    [Test]
    procedure fileExtensionAssignment;
    [Test]
    procedure filterTagAssignment;

    [Test]
//    [TestCase('TestImages with Absolute Path (no Network - No Filter)', 'C:\TestImages, , png')]
//    [TestCase('TestImages with Absolute Path (no Network - Filter)', 'C:\TestImages, action-repeat, png')]
    [TestCase('TestImages with Absolute Path (network - No Filter)',
      '\\vmware-host\Shared Folders\ProductionHD\NusEnvision\Components\Delphi\neImageTools\Development\Tests\TestImages, , png')]
    [TestCase('TestImages with Relative Path','..\..\..\TestImages, , png')]
    [TestCase('TestImages with Filter','..\..\..\TestImages, action_repeat, png')]
    procedure processDirectoriesTest(const aDirectory, aFilterTag, aExtension:
        string);

    [Test]
    procedure processImagesTest;

    [Test]
    procedure processImageListTest;
  end;

implementation

uses
  System.Generics.Collections,
  neImageList.Utils, System.SysUtils, System.IOUtils, neImageTools.Types;

procedure TneImageListTest.Setup;
begin
  fneImageList:=TneImageList.Create(nil);
end;

procedure TneImageListTest.TearDown;
begin
  fneImageList.Free;
end;

procedure TneImageListTest.baseScaleAssignment;
var
  current: single;
begin
  current:=fneImageList.BaseScale;
  fneImageList.BaseScale:=200;
  Assert.IsTrue(200 = fneImageList.BaseScale,'BaseScale not retrieved correctly');
  fneImageList.BaseScale:=0;
  Assert.IsTrue(defaultScale = fneImageList.BaseScale, 'Default scale not set when 0');
  fneImageList.BaseScale:=current;
end;

procedure TneImageListTest.delimiterAssignment;
var
  current: string;
begin
  current:=fneImageList.DelimiterString;
  fneImageList.DelimiterString:='NewDelim';
  Assert.AreEqual('NewDelim',fneImageList.DelimiterString,'DelimiterString not retrieved');
  fneImageList.DelimiterString:=current;
end;

procedure TneImageListTest.directoryAssignment;
var
  current: string;
begin
  current:=fneImageList.Directory;
  fneImageList.Directory:='NewDirectory';
  Assert.AreEqual('NewDirectory',fneImageList.Directory,'Directory not retrieved');
  fneImageList.Directory:=current;
end;

procedure TneImageListTest.fileExtensionAssignment;
var
  current: string;
begin
  current:=fneImageList.FileExtension;
  fneImageList.FileExtension:='bat';
  Assert.AreEqual('bat',fneImageList.FileExtension,'FileExtension not retrieved');
  fneImageList.FileExtension:=current;
end;

procedure TneImageListTest.filterTagAssignment;
var
  current: string;
begin
  current:=fneImageList.FilterTag;
  fneImageList.FilterTag:='add';
  Assert.AreEqual('add',fneImageList.FilterTag,'FilterTag not retrieved');
  fneImageList.FilterTag:=current;
end;

procedure TneImageListTest.OnLoadImages(Sender: TObject);
var
  messageSource,
  messageDestination: string;
begin
  Assert.AreEqual(2, fneImageList.Source.Count,
                          'Images for Source not loaded correctly');
  Assert.AreEqual(2, fneImageList.Source.Items[
                      fneImageList.Source.IndexOf('action_repeat')].
                                                        MultiResBitmap.Count,
                   'Destination images not loaded correctly for #0');
  Assert.AreEqual(3, fneImageList.Source.Items[
                      fneImageList.Source.IndexOf('add_button')].
                            MultiResBitmap.Count,
                   'Destination images not loaded correctly for #1');
end;

procedure TneImageListTest.processDirectoriesTest(const aDirectory, aFilterTag,
    aExtension: string);
var
  list: TList<string>;
begin
  list:=TList<string>.Create;
  try
    processDirectories(aDirectory, aFilterTag, aExtension, list);
    if trim(aFilterTag)='' then
      Assert.AreEqual(5, list.Count, 'Empty Filter does not work correctly');
    if Trim(aFilterTag)='action_repeat' then
    begin
      Assert.AreEqual(2, list.Count, 'Filter '+aFilterTag+' does not work correctly');
      Assert.AreEqual('action_repeatx1x.png', ExtractFileName(list.Items[0]),
        'file '+list.Items[0]+' not corrctly retrieved');
    end;
  finally
    list.Free;
  end;
end;

procedure TneImageListTest.processImageListTest;
var
  dict: TDictionary<string, TStringDynArray>;
  dynArray: TStringDynArray;
begin
  dict:=TDictionary<string, TStringDynArray>.Create;
  try
    SetLength(dynArray, 2);
    dynArray[0]:=TPath.combine('..\..\..\TestImages','action_repeatx1x.png');
    dynArray[1]:=TPath.combine('..\..\..\TestImages','action_repeatx1x5x.png');
    dict.Add('action_repeat', dynArray);

    SetLength(dynArray, 3);
    dynArray[0]:=TPath.combine('..\..\..\TestImages','add_buttonx1x.png');
    dynArray[1]:=TPath.combine('..\..\..\TestImages','add_buttonx1x5x.png');
    dynArray[2]:=TPath.combine('..\..\..\TestImages','add_buttonx1x25x.png');
    dict.Add('add_button', dynArray);

    fneImageList.Source.Clear;
    fneImageList.Destination.Clear;

    processImageList(dict, fneImageList.BaseScale, @fneImageList, OnLoadImages);
  finally
    dict.Free;
  end;
end;

procedure TneImageListTest.processImagesTest;
var
  list: TList<string>;
  dict: TDictionary<string, TStringDynArray>;
begin
  list:=TList<string>.Create;
  dict:=TDictionary<string, TStringDynArray>.Create;
  try
    list.Add(TPath.combine('..\..\..\TestImages', 'action_repeatx1x.png'));
    list.Add(TPath.combine('..\..\..\TestImages','action_repeatx1x5x.png'));
    list.Add(TPath.combine('..\..\..\TestImages','add_buttonx1x.png'));
    list.Add(TPath.combine('..\..\..\TestImages','add_buttonx1x5x.png'));
    list.Add(TPath.combine('..\..\..\TestImages','add_buttonx1x25x.png'));

    processImages('x', list, dict);

    Assert.IsTrue(dict.Keys.Count = 2, 'Image files not grouped correctly');
    Assert.AreEqual(2, Length(dict.Items['action_repeat']), 'Not right num of files for #0');
    Assert.AreEqual(3, Length(dict.Items['add_button']), 'Not right num of files for #1');

  finally
    dict.Free;
    list.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TneImageListTest);
end.
