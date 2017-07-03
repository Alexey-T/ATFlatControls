program Project1;

uses
  XPMan,
  Forms,
  DemoForm in 'DemoForm.pas' {FormDemo},
  ATScrollBar in 'ATScrollBar.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.
