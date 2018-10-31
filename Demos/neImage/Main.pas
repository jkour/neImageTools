unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  neImage;

type
  TForm3 = class(TForm)
    neImage1: TneImage;
    neImage2: TneImage;
    procedure FormCreate(Sender: TObject);
    procedure neImage2GetBitmap(aBitmap: TBitmap);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.FormCreate(Sender: TObject);
begin
  neImage1.Directory:='..\..\..\..\Images';
end;

procedure TForm3.neImage2GetBitmap(aBitmap: TBitmap);
begin
  aBitmap.LoadFromFile('..\..\..\..\Images\action_repeatx1x.png');
end;

end.
