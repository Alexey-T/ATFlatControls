program demo;

{$MODE Delphi}

uses
  {XPMan,}
  Forms, Interfaces,
  DemoForm in 'DemoForm.pas' {FormDemo};

{.$R *.res}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.
