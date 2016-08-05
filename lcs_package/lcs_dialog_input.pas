unit lcs_dialog_input;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Graphics, StdCtrls, ExtCtrls;

type

  { Tfrm_lcs_dialog_input }

  Tfrm_lcs_dialog_input = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    editText: TEdit;
    lblPrompt: TLabel;
    panelButtons: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frm_lcs_dialog_input: Tfrm_lcs_dialog_input;

implementation

{$R *.lfm}

{ Tfrm_lcs_dialog_input }

procedure Tfrm_lcs_dialog_input.FormCreate(Sender: TObject);
begin
  Self.ChildSizing.LeftRightSpacing := ScaleX(Self.ChildSizing.LeftRightSpacing, 96);
  Self.ChildSizing.TopBottomSpacing := ScaleY(Self.ChildSizing.TopBottomSpacing, 96);
  Self.ChildSizing.HorizontalSpacing := ScaleX(Self.ChildSizing.HorizontalSpacing, 96);
  Self.ChildSizing.VerticalSpacing := ScaleY(Self.ChildSizing.VerticalSpacing, 96);
  Self.panelButtons.ChildSizing.LeftRightSpacing := ScaleX(Self.panelButtons.ChildSizing.LeftRightSpacing, 96);
  Self.panelButtons.ChildSizing.TopBottomSpacing := ScaleY(Self.panelButtons.ChildSizing.TopBottomSpacing, 96);
  Self.panelButtons.ChildSizing.HorizontalSpacing := ScaleX(Self.panelButtons.ChildSizing.HorizontalSpacing, 96);
  Self.panelButtons.ChildSizing.VerticalSpacing := ScaleY(Self.panelButtons.ChildSizing.VerticalSpacing, 96);
  Self.Width := ScaleX(Self.Width, 96);
  Self.Height := ScaleY(Self.Height, 96);
end;

end.

