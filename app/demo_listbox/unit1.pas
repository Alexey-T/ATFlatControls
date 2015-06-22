unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ATListbox;

type
  { TfmMain }

  TfmMain = class(TForm)
    ed: TEdit;
    procedure edKeyDown(Sender: TObject; var Key: Word; Shift{%H-}: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure ListDraw(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure ListClick(Sender: TObject);
  public
    { public declarations }
    b: TATListbox;
  end;

var
  fmMain: TfmMain;

implementation

uses Math, LCLProc, LCLType;

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  b:= TATListbox.Create(Self);
  b.Parent:= Self;
  b.Align:= alClient;

  b.OnDrawItem:= @ListDraw;
  b.OnClick:= @ListClick;

  b.Color:= $e0e0e0;
  b.ItemCount:= 21;
end;

procedure TfmMain.ListDraw(Sender: TObject; C: TCanvas; AIndex: integer;
  const ARect: TRect);
begin
  C.Brush.Color:= IfThen(AIndex=b.ItemIndex, clMedGray, b.Color);
  C.FillRect(ARect);

  C.Pen.Color:= clMedGray;
  C.Line(ARect.Left+2, ARect.Bottom-1, ARect.Right-2, ARect.Bottom-1);

  C.TextOut(ARect.Left+6, ARect.Top+2, 'item '+inttostr(AIndex));
end;

procedure TfmMain.ListClick(Sender: TObject);
begin
  Beep;
  Caption:= 'Clicked: '+IntToStr(b.ItemIndex);
end;

procedure TfmMain.edKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key=vk_up) then
  begin
    b.ItemIndex:= b.ItemIndex-1;
    key:= 0;
    Exit
  end;
  if (key=vk_down) then
  begin
    b.ItemIndex:= b.ItemIndex+1;
    key:= 0;
    Exit
  end;

  if (key=vk_prior) then
  begin
    b.ItemIndex:= Max(0, b.ItemIndex-(b.VisibleItems-1));
    key:= 0;
    Exit
  end;
  if (key=vk_next) then
  begin
    b.ItemIndex:= Min(b.ItemCount-1, b.ItemIndex+(b.VisibleItems-1));
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

  if (key=vk_return) then
  begin
    ListClick(nil);
    key:= 0;
    Exit
  end;
end;

end.

