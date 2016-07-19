program lainzcs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, lcs_debugform;

{$R *.res}

begin
  Application.Title:='Lainz Code Studio';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.Run;
end.

