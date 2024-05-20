unit Controller.Login;

interface

uses
  Vcl.Forms, Vcl.Dialogs, Winapi.Windows, Winapi.Messages, System.Classes,
  System.SysUtils, System.StrUtils, System.Variants, Framework.Factory,
  FireDAC.Comp.Client, FireDAC.Stan.Param, Dao.Conexao, View.Login;

type
  TLoginController = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FView : TFrmLogin;
    FFactory : TFactory;
    FTentativas : Integer;
    function IsValideUser(AUserName, ASenha : String) : Boolean;
    procedure btnLogarOnClick(Sender: TObject);
    procedure btnCancelarOnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TLoginController }

const
  NTentativas = 3;

procedure TLoginController.btnCancelarOnClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TLoginController.btnLogarOnClick(Sender: TObject);
begin
  if String(Self.FView.edtUsuario.Text).IsEmpty or String(Self.FView.edtSenha.Text).IsEmpty then
  begin
    Self.FView.edtUsuario.SetFocus;
    Exit;
  end;
  if not IsValideUser(Self.FView.edtUsuario.Text, Self.FView.edtSenha.Text) then
  begin
    Self.FView.edtUsuario.SetFocus;
    Abort;
  end;
  Self.FView.Close;
end;

constructor TLoginController.Create;
begin
  Self.FStrB                     := TStringBuilder.Create;
  Self.FQuery                    := TFDQuery.Create(nil);
  Self.FQuery.Connection         := DMConexao.Conexao;
  Self.FView                     := TFrmLogin.Create(nil);
  Self.FView.OnKeyPress          := Self.FormKeyPress;
  Self.FFactory                  := TFactory.Create;
  Self.FView.btnLogar.OnClick    := Self.btnLogarOnClick;
  Self.FView.btnCancelar.OnClick := Self.btnCancelarOnClick;
  Self.FView.ShowModal;
end;

destructor TLoginController.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FView);
  FreeAndNil(Self.FFactory);
  inherited;
end;

procedure TLoginController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

function TLoginController.IsValideUser(AUserName, ASenha: String): Boolean;
begin
  Result := True;
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT TX_SENHA               ')
            .AppendLine('  FROM USUARIO                ')
            .AppendLine('  WHERE TX_LOGIN = :TX_LOGIN; ');

  Self.FQuery.Close;
  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text := Self.FStrB.ToString;
  Self.FQuery.ParamByName('TX_LOGIN').AsString := AUserName;
  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.Open;
    Self.FQuery.Connection.Commit;
  except
    Self.FQuery.Connection.Rollback;
  end;

  if Self.FQuery.FieldByName('TX_SENHA').IsNull then
  begin
    Inc(FTentativas);
    Application.MessageBox('O usuário ou senha inválido', 'Atenção', MB_OK+MB_ICONERROR);
    Result := False;
  end else if Self.FQuery.FieldByName('TX_SENHA').AsString <> Self.FFactory.GetStrHashSHA512_256(ASenha) then
  begin
    Inc(FTentativas);
    Application.MessageBox('O usuário ou senha inválido', 'Atenção', MB_OK+MB_ICONERROR);
    Result := False;
  end;

  if Self.FTentativas = NTentativas then
    Application.Terminate;
end;

end.
