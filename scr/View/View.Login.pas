unit View.Login;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFrmLogin = class(TForm)
    edtUsuario: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtSenha: TEdit;
    btnLogar: TButton;
    btnCancelar: TButton;
    Panel1: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

initialization
  RegisterClass(TFrmLogin);

Finalization
  unRegisterClass(TFrmLogin);

end.
