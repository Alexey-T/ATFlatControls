unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ATListbox, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    L: TATListbox;
    Panel1: TPanel;
    chkThemed: TCheckBox;
    chkHorzBar: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure chkThemedClick(Sender: TObject);
    procedure chkHorzBarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.chkHorzBarClick(Sender: TObject);
begin
  L.ShowHorzScrollbar:= chkHorzBar.Checked;
  L.Invalidate;
end;

procedure TForm1.chkThemedClick(Sender: TObject);
begin
  L.ThemedScrollbar:= chkThemed.Checked;
  L.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  L.VirtualMode:= false;
  L.HotTrack:= true;
  for i := 0 to 100 do
    L.Items.Add(IntToStr(i)+') '+StringOfChar('n', Random(200)));
  L.Invalidate;
end;

end.
