unit Controller.Usuario;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Generics.Collections, System.StrUtils, Framework.Factory, Model.Usuario,
  View.Usuario, Dao.Usuario, FireDAC.Comp.Client, FireDAC.Stan.Param, Vcl.Dialogs,
  System.Variants;

type
  TUsuarioController = class
  private
    FView : TFrmUsuario;
    FFactory : TFactory;
    FStatus : TStatus;
    FUsuarioModel : TUsuarioModel;
    FUsuarioDao : TUsuarioDao;
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
    if Self.FUsuarioDao.Delete(Self.FView.FDMUsuarioCO_USUARIO.AsInteger) then
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
      begin
        Self.FUsuarioModel.co_usuario   := Self.FView.FDMUsuarioCO_USUARIO.AsInteger;
        Self.FUsuarioModel.dt_alteracao := Self.FUsuarioDao.GetDateTime;
      end
      else
        Self.FUsuarioModel.co_usuario := StrToInt(Self.FView.edtCodigo.Text);
      Self.FUsuarioModel.nm_usuario   := Self.FView.edtNomeUsuario.Text;
      Self.FUsuarioModel.tx_login     := Self.FView.edtLogin.Text;
      Self.FUsuarioModel.tx_senha     := Self.FView.edtSenha.Text;
      Self.FUsuarioModel.in_ativo     := IfThen(Self.FView.rdgSituacao.ItemIndex = 0, 'A', 'I');


      if Self.FStatus = TStatus.dsInsert then
      begin

        Self.FUsuarioDao.Insert(Self.FUsuarioModel);

        if not Self.FView.FDMUsuario.Active then
          Self.FView.FDMUsuario.Open;

        Self.FView.FDMUsuario.Insert;
        Self.FView.FDMUsuarioCO_USUARIO.AsInteger := StrToInt64(Self.FView.edtCodigo.Text);
        Self.FView.FDMUsuarioNM_USUARIO.AsString  := Self.FView.edtNomeUsuario.Text;
        Self.FView.FDMUsuarioTX_LOGIN.AsString    := Self.FView.edtLogin.Text;
        Self.FView.FDMUsuarioTX_SENHA.AsString    := Self.FFactory.GetStrHashSHA512_256(Self.FView.edtSenha.Text);
        Self.FView.FDMUsuarioIN_ATIVO.AsString    := IfThen(Self.FView.rdgSituacao.ItemIndex = 0, 'A', 'I');
        Self.FView.FDMUsuario.Post;

      end
      else if Self.FStatus = TStatus.dsEdit then
        Self.FUsuarioDao.Update(Self.FUsuarioModel);

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
  Self.FFactory.ClearControls(Self.FView);
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabCadastro;
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtLogin.SetFocus;
  Self.FView.edtCodigo.Text := FormatFloat('000000', Self.FUsuarioDao.GetNewID('INC_CO_USUARIO'));
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

procedure TUsuarioController.btnVoltarClick(Sender: TObject);
begin
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
end;

constructor TUsuarioController.Create;
begin
  Self.FFactory                           := TFactory.Create;
  Self.FUsuarioModel                      := TUsuarioModel.Create;
  Self.FUsuarioDao                        := TUsuarioDao.Create;
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

destructor TUsuarioController.Destroy;
begin
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FUsuarioModel);
  FreeAndNil(Self.FUsuarioDao);
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
    end else if not (Self.FUsuarioDao.ValidateUser(Self.FView.edtLogin.Text)) then
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
  LUsuarios := Self.FUsuarioDao.GetAll;
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

end.
