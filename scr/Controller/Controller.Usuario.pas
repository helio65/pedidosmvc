unit Controller.Usuario;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Generics.Collections, System.StrUtils, Framework.Factory, Model.Usuario, View.Usuario,
  FireDAC.Comp.Client, FireDAC.Stan.Param, Dao.Conexao, Vcl.Dialogs,
  System.Variants;

type
  TUsuarioController = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FView : TFrmUsuario;
    FFactory : TFactory;
    FStatus : TStatus;
    FUsuarioModel : TUsuarioModel;
    procedure btnFecharClick(Sender : TObject);
    procedure btnNovoClick(Sender : TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGravarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnApagarClick(Sender: TObject);
    procedure btnVoltarClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure gridBaseDblClick(Sender: TObject);
    procedure edtLoginOnExit(Sender: TObject);
    procedure edtSenhaOnExit(Sender: TObject);
  public
    procedure Insert;
    function Delete(co_usuario : Integer) : Boolean;
    procedure Save(aObject : TUsuarioModel);
    function GetAll : TObjectList<TUsuarioModel>;
    function ValidateUser(AUserName : String) : Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TUsuarioController }

procedure TUsuarioController.btnApagarClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma a exclusão desta Empresa?', 'Atenção', MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = IDYES then
  begin
    Self.FStatus := TStatus.dsDelete;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    if Self.Delete(Self.FView.FDMUsuarioCO_USUARIO.AsInteger) then
    begin
      if Self.FView.pgcPrincipal.ActivePage = Self.FView.tabCadastro then
      begin
        Self.FFactory.ClearControls(Self.FView);
        Self.FormShow(Sender);
      end else
      begin
        Self.FormShow(Sender);
      end;
    end else
    begin
      Self.FormShow(Sender);
    end;
  end;
end;

procedure TUsuarioController.btnCancelarClick(Sender: TObject);
begin
  if Self.FStatus = TStatus.dsEdit then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FView.edtCodigo.Text        := FormatFloat('000000', Self.FView.FDMUsuarioCO_USUARIO.AsInteger);
    Self.FView.edtNomeUsuario.Text   := Self.FView.FDMUsuarioNM_USUARIO.AsString;
    Self.FView.edtLogin.Text         := Self.FView.FDMUsuarioTX_LOGIN.AsString;
    Self.FView.edtSenha.Text         := Self.FView.FDMUsuarioTX_SENHA.AsString;
    Self.FView.rdgSituacao.ItemIndex := IfThen(Self.FView.FDMUsuarioIN_ATIVO.AsString = 'A', '0', '1').ToInteger;
    Self.FStatus                     := TStatus.dsBrowse;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end else if Self.FStatus = TStatus.dsInsert then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FStatus                       := TStatus.dsBrowse;
    Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end;
end;

procedure TUsuarioController.btnEditarClick(Sender: TObject);
begin
  Self.FStatus := TStatus.dsEdit;
  if Self.FView.pgcPrincipal.ActivePage = Self.FView.tabConsulta then
    Self.gridBaseDblClick(Sender);
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
end;

procedure TUsuarioController.btnFecharClick(Sender: TObject);
begin
  Self.FView.Close;
end;

procedure TUsuarioController.btnGravarClick(Sender: TObject);
var
  aListError : TStringList;
begin
  aListError := TStringList.Create;
  aListError.Add('Lista de erro(s):');
  aListError.Add('');
  try
    if String(Self.FView.edtLogin.Text).IsEmpty then
      aListError.Add('CNPJ');
    if String(Self.FView.edtNomeUsuario.Text).IsEmpty then
      aListError.Add('Razão Social');
    if String(Self.FView.edtSenha.Text).IsEmpty then
      aListError.Add('Senha');
    if Self.FView.rdgSituacao.ItemIndex = -1 then
      aListError.Add('Situação');

    if aListError.Count > 2 then
    begin
      aListError.Add('');
      aListError.Add('O(s) campo(s) listado(s) acima são de preenchimento obrigatório.');

      Application.MessageBox(PChar(aListError.Text), 'Atenção', MB_OK+MB_ICONERROR);
      Self.FView.edtLogin.SetFocus;
    end else
    begin
      if Self.FStatus = TStatus.dsEdit then
        Self.FUsuarioModel.co_usuario := Self.FView.FDMUsuarioCO_USUARIO.AsInteger
      else
        Self.FUsuarioModel.co_usuario := StrToInt(Self.FView.edtCodigo.Text);
      Self.FUsuarioModel.nm_usuario   := Self.FView.edtNomeUsuario.Text;
      Self.FUsuarioModel.tx_login     := Self.FView.edtLogin.Text;
      Self.FUsuarioModel.tx_senha     := Self.FView.edtSenha.Text;
      Self.FUsuarioModel.in_ativo     := IfThen(Self.FView.rdgSituacao.ItemIndex = 0, 'A', 'I');
      Self.FUsuarioModel.dt_alteracao := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
      Self.Save(Self.FUsuarioModel);
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    end;
  finally
    FreeAndNil(aListError);
  end;
end;

procedure TUsuarioController.btnNovoClick(Sender: TObject);
begin
  Self.Insert;
end;

procedure TUsuarioController.btnVoltarClick(Sender: TObject);
begin
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
end;

constructor TUsuarioController.Create;
begin
  Self.FStrB                              := TStringBuilder.Create;
  Self.FQuery                             := TFDQuery.Create(nil);
  Self.FQuery.Connection                  := DMConexao.Conexao;
  Self.FFactory                           := TFactory.Create;
  Self.FUsuarioModel                      := TUsuarioModel.Create;
  Self.FView                              := TFrmUsuario.Create(nil);
  Self.FView.KeyPreview                   := True;
  Self.FView.btnFechar.OnClick            := Self.btnFecharClick;
  Self.FView.btnNovo.OnClick              := Self.btnNovoClick;
  Self.FView.btnEditar.OnClick            := Self.btnEditarClick;
  Self.FView.btnGravar.OnClick            := Self.btnGravarClick;
  Self.FView.btnVoltar.OnClick            := Self.btnVoltarClick;
  Self.FView.OnShow                       := Self.FormShow;
  Self.FView.OnKeyPress                   := Self.FormKeyPress;
  Self.FView.btnCancelar.OnClick          := Self.btnCancelarClick;
  Self.FView.btnApagar.OnClick            := Self.btnApagarClick;
  Self.FView.gridBase.OnDblClick          := Self.gridBaseDblClick;
  Self.FView.edtLogin.OnExit              := Self.edtLoginOnExit;
  Self.FView.edtSenha.OnExit              := Self.edtSenhaOnExit;
  Self.FView.btnNovo.ShowHint             := True;
  Self.FView.btnNovo.Hint                 := 'Adicionar um novo registro';
  Self.FView.btnEditar.ShowHint           := True;
  Self.FView.btnEditar.Hint               := 'Editar o registro corrente';
  Self.FView.btnGravar.ShowHint           := True;
  Self.FView.btnGravar.Hint               := 'Gravar o registro corrente';
  Self.FView.btnCancelar.ShowHint         := True;
  Self.FView.btnCancelar.Hint             := 'Cnacelar a operação corrente';
  Self.FView.btnApagar.ShowHint           := True;
  Self.FView.btnApagar.Hint               := 'Apagar o registro corrente';
  Self.FView.btnFechar.ShowHint           := True;
  Self.FView.btnFechar.Hint               := 'Fechar';
  Self.FFactory.ControlControls(FView, TStatus.dsInactive);
  Self.FView.ShowModal;
end;

function TUsuarioController.Delete(co_usuario: Integer): Boolean;
begin
  Self.FStatus := TStatus.dsDelete;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('DELETE FROM USUARIO               ')
            .AppendLine('  WHERE CO_USUARIO = :CO_USUARIO; ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_EMPRESA').AsInteger := co_usuario;
  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.ExecSQL;
    Self.FQuery.Connection.Commit;
    Result := True;
  except
    on e : exception do
    begin
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FQuery.Connection.Rollback;
      raise Exception.Create(e.Message);
    end;
  end;
end;

destructor TUsuarioController.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FUsuarioModel);
  FreeAndNil(Self.FView);
  inherited;
end;

procedure TUsuarioController.edtLoginOnExit(Sender: TObject);
begin
  if ((Self.FStatus = TStatus.dsEdit) or (Self.FStatus = TStatus.dsInsert)) and String(Self.FView.edtLogin.Text).IsEmpty then
  begin
    if String(Self.FView.edtLogin.Text).Length < 8 then
    begin
      Application.MessageBox('Seu login teve conter no mínimo 8 caractes e no máximo 20.', 'Atenção', MB_OK+MB_ICONWARNING);
      Self.FView.edtLogin.SetFocus;
      Abort;
    end else if not (Self.ValidateUser(Self.FView.edtLogin.Text)) then
    begin
      Application.MessageBox('Já existe um usuário cadastro com o Login informado!', 'Atenção', MB_OK+MB_ICONWARNING);
      Self.FView.edtLogin.SetFocus;
      Abort;
    end;
  end;
end;

procedure TUsuarioController.edtSenhaOnExit(Sender: TObject);
var
  LSenha : String;
begin
  if ((Self.FStatus = TStatus.dsEdit) or (Self.FStatus = TStatus.dsInsert)) and not String(Self.FView.edtSenha.Text).IsEmpty then
  begin
    if String(Self.FView.edtSenha.Text).Length < 8 then
    begin
      Application.MessageBox('Informe uma senha com no mínimo 8 caracates e no máximo 15.', 'Atenção', MB_OK+MB_ICONWARNING);
      Self.FView.edtSenha.SetFocus;
      Abort;
    end else begin
      if not InputQuery('Cornfirmar Senha', #0'Redigite a Senha', LSenha) then
      begin
        Application.MessageBox('Senha não confirmada', 'Atenção', MB_OK+MB_ICONWARNING);
        Self.FView.edtSenha.SetFocus;
        Abort;
      end else if LSenha <> Self.FView.edtSenha.Text then
      begin
        Application.MessageBox('Senhas não conferem.', 'Atenção', MB_OK+MB_ICONERROR);
        Self.FView.edtSenha.SetFocus;
        Abort;
      end;
    end;
  end;
end;

procedure TUsuarioController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

procedure TUsuarioController.FormShow(Sender: TObject);
var
  LUsuarios : TObjectList<TUsuarioModel>;
  aIndex: Integer;
begin
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
  LUsuarios := Self.GetAll;
  try
    if LUsuarios.Count > 0 then
    begin
      Self.FView.FDMUsuario.Open;
      Self.FView.FDMUsuario.EmptyDataSet;
      for aIndex := 0 to Pred(LUsuarios.Count) do
      begin
        Self.FView.FDMUsuario.Insert;
        Self.FView.FDMUsuarioCO_USUARIO.AsInteger   := LUsuarios[aIndex].co_usuario;
        Self.FView.FDMUsuarioNM_USUARIO.AsString    := LUsuarios[aIndex].nm_usuario;
        Self.FView.FDMUsuarioTX_LOGIN.AsString      := LUsuarios[aIndex].tx_login;
        Self.FView.FDMUsuarioTX_SENHA.AsString      := LUsuarios[aIndex].tx_senha;
        Self.FView.FDMUsuarioIN_ATIVO.AsString      := LUsuarios[aIndex].in_ativo;
        Self.FView.FDMUsuarioDT_CADASTRO.AsDateTime := LUsuarios[aIndex].dt_cadastro;

        if not Self.FFactory.DateTimeIsNull(LUsuarios[aIndex].dt_alteracao) then
          Self.FView.FDMUsuarioDT_ALTERACAO.AsDateTime  := LUsuarios[aIndex].dt_alteracao;
        Self.FView.FDMUsuario.Post;
      end;
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    end else begin
      Self.FStatus := TStatus.dsInactive;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FView.FDMUsuario.Close;
    end;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  finally
    FreeAndNil(LUsuarios);
  end;
end;

function TUsuarioController.GetAll: TObjectList<TUsuarioModel>;
var
  LUsuarios : TUsuarioModel;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT CO_USUARIO, NM_USUARIO, TX_LOGIN, TX_SENHA, IN_ATIVO, ')
            .AppendLine('       DT_CADASTRO, DT_ALTERACAO                             ')
            .AppendLine('  FROM USUARIO                                               ')
            .AppendLine('  ORDER BY NM_USUARIO;                                       ');

  Self.FQuery.Close;
  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text := Self.FStrB.ToString;
  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.Open;
    Self.FQuery.Connection.Commit;
  except
    Self.FQuery.Connection.Rollback;
  end;
  Result := TObjectList<TUsuarioModel>.Create;
  if Self.FQuery.RecordCount > 0 then
  begin
    while not Self.FQuery.Eof do
    begin
      LUsuarios             := TUsuarioModel.Create;
      LUsuarios.co_usuario  := Self.FQuery.FieldByName('CO_USUARIO').AsInteger;
      LUsuarios.nm_usuario  := Self.FQuery.FieldByName('NM_USUARIO').AsString;
      LUsuarios.tx_login    := Self.FQuery.FieldByName('TX_LOGIN').AsString;
      LUsuarios.tx_senha    := Self.FQuery.FieldByName('TX_SENHA').AsString;
      LUsuarios.in_ativo    := Self.FQuery.FieldByName('IN_ATIVO').AsString;
      LUsuarios.dt_cadastro := Self.FQuery.FieldByName('DT_CADASTRO').AsDateTime;
      if not Self.FQuery.FieldByName('DT_ALTERACAO').IsNull then
        LUsuarios.dt_alteracao    := Self.FQuery.FieldByName('DT_ALTERACAO').AsDateTime;
      Result.Add(LUsuarios);
      Self.FQuery.Next;
    end;
  end;
end;

procedure TUsuarioController.gridBaseDblClick(Sender: TObject);
begin
  if Self.FView.FDMUsuario.RecordCount = 0 then
    Exit;
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabCadastro;
  Self.FView.edtCodigo.Text          := FormatFloat('000000', Self.FView.FDMUsuarioCO_USUARIO.AsInteger);
  Self.FView.edtNomeUsuario.Text     := Self.FView.FDMUsuarioNM_USUARIO.AsString;
  Self.FView.edtLogin.Text           := Self.FView.FDMUsuarioTX_LOGIN.AsString;
  Self.FView.edtSenha.Text           := Self.FView.FDMUsuarioTX_SENHA.Text;
  Self.FView.rdgSituacao.ItemIndex   := IfThen(Self.FView.FDMUsuarioIN_ATIVO.Text = 'A', '0', '1').ToInteger;
  if Self.FStatus = TStatus.dsEdit then
    Exit;
  Self.FStatus                       := TStatus.dsBrowse;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aDisable);
end;

procedure TUsuarioController.Insert;
begin
  Self.FFactory.ClearControls(Self.FView);
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabCadastro;
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtLogin.SetFocus;
  Self.FView.edtCodigo.Text := FormatFloat('000000', Self.FFactory.GetNextID('INC_CO_USUARIO', DMConexao.Conexao));
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

procedure TUsuarioController.Save(aObject: TUsuarioModel);
begin
  if Self.FStatus = TStatus.dsInsert then
  begin
    Self.FStrB.Clear;
    Self.FStrB.AppendLine('INSERT INTO USUARIO (CO_USUARIO, NM_USUARIO, TX_LOGIN, TX_SENHA, IN_ATIVO)       ')
              .AppendLine('             VALUES (:CO_USUARIO, :NM_USUARIO, :TX_LOGIN, :TX_SENHA, :IN_ATIVO); ');

    Self.FQuery.SQL.Clear;
    Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
    Self.FQuery.ParamByName('CO_USUARIO').AsInteger := aObject.co_usuario;
    Self.FQuery.ParamByName('NM_USUARIO').AsString  := aObject.nm_usuario;
    Self.FQuery.ParamByName('TX_LOGIN').AsString    := aObject.tx_login;
    Self.FQuery.ParamByName('TX_SENHA').AsString    := Self.FFactory.GetStrHashSHA512_256(aObject.tx_senha);
    Self.FQuery.ParamByName('IN_ATIVO').AsString    := aObject.in_ativo;

    if not Self.FView.FDMUsuario.Active then
      Self.FView.FDMUsuario.Open;

    Self.FView.FDMUsuario.Insert;
    Self.FView.FDMUsuarioCO_USUARIO.AsInteger := aObject.co_usuario;
    Self.FView.FDMUsuarioNM_USUARIO.AsString  := aObject.nm_usuario;
    Self.FView.FDMUsuarioTX_LOGIN.AsString    := aObject.tx_login;
    Self.FView.FDMUsuarioTX_SENHA.AsString    := Self.FFactory.GetStrHashSHA512_256(aObject.tx_senha);
    Self.FView.FDMUsuarioIN_ATIVO.AsString    := aObject.in_ativo;
    Self.FView.FDMUsuario.Post;

    if not Self.FQuery.Connection.InTransaction then
      Self.FQuery.Connection.StartTransaction;
    try
      Self.FQuery.ExecSQL;
      Self.FQuery.Connection.Commit;
    except
      on e : exception do
      begin
        Self.FQuery.Connection.Rollback;
        raise Exception.Create(e.Message);
      end;
    end;
  end else if Self.FStatus = TStatus.dsEdit then
  begin
    Self.FStrB.Clear;
    Self.FStrB.AppendLine('UPDATE USUARIO                     ')
              .AppendLine('  SET NM_USUARIO   = :NM_USUARIO,  ')
              .AppendLine('      IN_ATIVO     = :IN_ATIVO,    ')
              .AppendLine('      DT_ALTERACAO = :DT_ALTERACAO ')
              .AppendLine('  WHERE CO_USUARIO = :CO_USUARIO;  ');

    Self.FQuery.SQL.Clear;
    Self.FQuery.SQL.Text                               := Self.FStrB.ToString;
    Self.FQuery.ParamByName('NM_USUARIO').AsString     := aObject.nm_usuario;
    Self.FQuery.ParamByName('IN_ATIVO').AsString       := aObject.in_ativo;
    Self.FQuery.ParamByName('DT_ALTERACAO').AsDateTime := aObject.dt_alteracao;
    Self.FQuery.ParamByName('CO_USUARIO').AsInteger    := aObject.co_usuario;

    if not Self.FQuery.Connection.InTransaction then
      Self.FQuery.Connection.StartTransaction;
    try
      Self.FQuery.ExecSQL;
      Self.FQuery.Connection.Commit;
    except
      on e : exception do
      begin
        Self.FQuery.Connection.Rollback;
        raise Exception.Create(e.Message);
      end;
    end;
  end;
end;

function TUsuarioController.ValidateUser(AUserName: String): Boolean;
begin
  Result := False;
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT TX_LOGIN               ')
            .AppendLine('  FROM USUARIO;               ')
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
    if Self.FQuery.FieldByName('TX_LOGIN').IsNull then
      Result := True;
  except
    Self.FQuery.Connection.Rollback;
  end;
end;

end.
