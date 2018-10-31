program Demo.neImage;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form3},
  neImage in '..\..\SourceCode\Common\neImage.pas',
  neImageTools.Types in '..\..\SourceCode\Common\neImageTools.Types.pas',
  neImageList.Utils in '..\..\SourceCode\Common\neImageList.Utils.pas',
  neImageList in '..\..\SourceCode\Common\neImageList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
