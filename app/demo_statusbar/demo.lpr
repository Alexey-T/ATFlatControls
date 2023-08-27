program demo;

uses
  Interfaces,
  Forms,
  DemoForm in 'DemoForm.pas' {Form1},
  ATStatusBar;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;  
end.
