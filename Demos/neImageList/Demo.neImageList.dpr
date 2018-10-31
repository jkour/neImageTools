program Demo.neImageList;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form2},
  neImageList.Utils in '..\..\SourceCode\Common\neImageList.Utils.pas',
  neImageList in '..\..\SourceCode\Common\neImageList.pas',
  neImageTools.Types in '..\..\SourceCode\Common\neImageTools.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
