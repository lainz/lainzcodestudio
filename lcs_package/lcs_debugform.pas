unit lcs_debugform;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Graphics, StdCtrls;

type

  { TfrmDebug }

  TfrmDebug = class(TForm)
    memoDebug: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDebug: TfrmDebug;

implementation

{$R *.lfm}

{ TfrmDebug }

procedure TfrmDebug.FormCreate(Sender: TObject);
begin
  Self.ChildSizing.LeftRightSpacing := ScaleX(Self.ChildSizing.LeftRightSpacing, 96);
  Self.ChildSizing.TopBottomSpacing := ScaleY(Self.ChildSizing.TopBottomSpacing, 96);
  Self.Width := ScaleX(Self.Width, 96);
  Self.Height := ScaleY(Self.Height, 96);
end;

end.

