unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ATListbox;

type
  { TForm1 }

  TForm1 = class(TForm)
    ed: TEdit;
    procedure edKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure DrawItem(Sender: TObject; AIndex: integer; const ARect: TRect);
  public
    { public declarations }
    b: TATListbox;
  end;

var
  Form1: TForm1;

implementation

uses Math, LCLProc, LCLType;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  b:= TATListbox.Create(Self);
  b.Parent:= Self;
  b.Align:= alClient;
  b.OnDrawItem:= @DrawItem;

  b.ItemCount:= 20;
end;

procedure TForm1.DrawItem(Sender: TObject; AIndex: integer; const ARect: TRect);
var
  cl: TColor;
begin
  cl:= IfThen(AIndex=b.ItemIndex, clGray, b.ColorBg);
  b.Canvas.Brush.Color:= cl;
  b.Canvas.FillRect(ARect);
  b.Canvas.TextOut(ARect.Left+4, ARect.Top+2, 'item '+inttostr(AIndex));
end;

procedure TForm1.edKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key=vk_down) then
  begin
    if b.ItemIndex<b.ItemCount-1 then
      b.ItemIndex:= b.ItemIndex+1;
    key:= 0;
    Exit
  end;
  if (key=vk_up) then
  begin
    if b.ItemIndex>0 then
      b.ItemIndex:= b.ItemIndex-1;
    key:= 0;
    Exit
  end;
  if (key=vk_home) then
  begin
    b.ItemIndex:= 0;
    key:= 0;
    Exit
  end;
  if (key=vk_end) then
  begin
    b.ItemIndex:= b.ItemCount-1;
    key:= 0;
    Exit
  end;
end;

end.

