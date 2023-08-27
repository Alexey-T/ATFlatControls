program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, unit_frame1, unit_frame2, unit_frame3
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormFrame1, FormFrame1);
  Application.CreateForm(TFormFrame2, FormFrame2);
  Application.CreateForm(TFormFrame3, FormFrame3);
  Application.Run;
end.

