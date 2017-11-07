unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ATGauge;

type

  { TForm1 }

  TForm1 = class(TForm)
    Gauge1: TGauge;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  with Gauge1 do
    if progress>=MaxValue then
    begin
      progress:= MinValue;
      if kind=gkHorizontalBar then
        kind:= gkVerticalBar
      else
      if kind=gkVerticalBar then
        kind:= gkPie
      else
      if kind=gkPie then
        kind:= gkHorizontalBar;
    end
    else
      AddProgress(2);
end;

end.

