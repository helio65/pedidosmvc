unit Controller.Empresa;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Generics.Collections, FireDAC.Comp.Client, FireDAC.Stan.Param, Vcl.Dialogs,
  System.JSON, System.Variants, REST.Types, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, Framework.Factory,
  Dao.Conexao, Dao.Empresa, Model.Empresa, View.Empresa;

type
  TEmpresaController = class
  private
    FView : TFrmEmpresa;
    FFactory : TFactory;
    FStatus : TStatus;
    FEmpresaModel : TEmpresaModel;
    FEmpresaDao : TEmpresaDao;
    procedure btnFecharClick(Sender : TObject);
    procedure btnNovoClick(Sender : TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGravarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnApagarClick(Sender: TObject);
    procedure btnVoltarClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure GetDadosEmpresa(const AValue : String);
    procedure edtCNPJonExit(Sender: TObject);
    procedure gridBaseDblClick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEmpresaController }

procedure TEmpresaController.btnApagarClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma a exclusão desta Empresa?', 'Atenção', MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = IDYES then
  begin
    Self.FStatus := TStatus.dsDelete;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    if Self.FEmpresaDao.Delete(Self.FView.FDMEmpresaCO_EMPRESA.AsInteger) then
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

procedure TEmpresaController.btnCancelarClick(Sender: TObject);
begin
  if Self.FStatus = TStatus.dsEdit then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FView.edtCodigo.Text      := FormatFloat('000000', Self.FView.FDMEmpresaCO_EMPRESA.AsInteger);
    Self.FView.edtCNPJ.Text        := Self.FView.FDMEmpresaNU_CNPJ.AsString;
    Self.FView.edtRazaoSocial.Text := Self.FView.FDMEmpresaNM_RAZAO_SOCIAL.AsString;
    Self.FView.edtEmail.Text       := Self.FView.FDMEmpresaTX_EMAIL.AsString;
    Self.FStatus                   := TStatus.dsBrowse;
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

procedure TEmpresaController.btnEditarClick(Sender: TObject);
begin
  Self.FStatus := TStatus.dsEdit;
  if Self.FView.pgcPrincipal.ActivePage = Self.FView.tabConsulta then
    Self.gridBaseDblClick(Sender);
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
end;

procedure TEmpresaController.btnFecharClick(Sender: TObject);
begin
  Self.FView.Close;
end;

procedure TEmpresaController.btnGravarClick(Sender: TObject);
var
  aListError : TStringList;
begin
  aListError := TStringList.Create;
  aListError.Add('Lista de erro(s):');
  aListError.Add('');
  try
    if String(Self.FView.edtCNPJ.Text).IsEmpty then
      aListError.Add('CNPJ');
    if String(Self.FView.edtRazaoSocial.Text).IsEmpty then
      aListError.Add('Razão Social');

    if aListError.Count > 2 then
    begin
      aListError.Add('');
      aListError.Add('O(s) campo(s) listado(s) acima são de preenchimento obrigatório.');

      Application.MessageBox(PChar(aListError.Text), 'Atenção', MB_OK+MB_ICONERROR);
      Self.FView.edtCNPJ.SetFocus;
    end else
    begin
      if Self.FStatus = TStatus.dsEdit then
      begin
        Self.FEmpresaModel.co_empresa := Self.FView.FDMEmpresaCO_EMPRESA.AsInteger;
        Self.FEmpresaModel.dt_alteracao    := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
      end
      else
        Self.FEmpresaModel.co_empresa    := StrToInt(Self.FView.edtCodigo.Text);

      Self.FEmpresaModel.nm_razao_social := Self.FView.edtRazaoSocial.Text;
      Self.FEmpresaModel.nu_cnpj         := Self.FView.edtCNPJ.Text;
      Self.FEmpresaModel.tx_email        := Self.FView.edtEmail.Text;

      if Self.FStatus = TStatus.dsInsert then
      begin

        Self.FEmpresaDao.Insert(Self.FEmpresaModel);

        if not Self.FView.FDMEmpresa.Active then
          Self.FView.FDMEmpresa.Open;

        Self.FView.FDMEmpresa.Insert;
        Self.FView.FDMEmpresaCO_EMPRESA.AsInteger     := StrtoInt(Self.FView.edtCodigo.Text);
        Self.FView.FDMEmpresaNM_RAZAO_SOCIAL.AsString := Self.FView.edtRazaoSocial.Text;
        Self.FView.FDMEmpresaNU_CNPJ.AsString         := Self.FView.edtCNPJ.Text;
        Self.FView.FDMEmpresaTX_EMAIL.AsString        := Self.FView.edtEmail.Text;
        Self.FView.FDMEmpresa.Post;

      end else if Self.FStatus = TStatus.dsEdit then
        Self.FEmpresaDao.Update(Self.FEmpresaModel);


      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    end;
  finally
    FreeAndNil(aListError);
  end;
end;

procedure TEmpresaController.btnNovoClick(Sender: TObject);
begin
  Self.FFactory.ClearControls(Self.FView);
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabCadastro;
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtCNPJ.SetFocus;
  Self.FView.edtCodigo.Text := FormatFloat('000000', Self.FEmpresaDao.GetNewID('INC_CO_EMPRESA'));
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

procedure TEmpresaController.btnVoltarClick(Sender: TObject);
begin
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
end;

constructor TEmpresaController.Create;
begin
  Self.FEmpresaDao                        := TEmpresaDao.Create;
  Self.FFactory                           := TFactory.Create;
  Self.FEmpresaModel                      := TEmpresaModel.Create;
  Self.FView                              := TFrmEmpresa.Create(nil);
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
  Self.FView.edtCNPJ.OnExit               := Self.edtCNPJonExit;
  Self.FView.gridBase.OnDblClick          := Self.gridBaseDblClick;
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

destructor TEmpresaController.Destroy;
begin
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FEmpresaModel);
  FreeandNil(Self.FEmpresaDao);
  FreeAndNil(Self.FView);
  inherited;
end;

procedure TEmpresaController.edtCNPJonExit(Sender: TObject);
begin
  if not String(Self.FView.edtCNPJ.Text).IsEmpty and ((Self.FStatus = TStatus.dsEdit) or (Self.FStatus = TStatus.dsInsert)) then
  begin
    if String(Self.FView.edtCNPJ.Text).Length <> 14 then
    begin
      Application.MessageBox('CNPJ informado não é válido, verifique.', 'Atenção', MB_OK+MB_ICONERROR);
      Self.FView.edtCNPJ.SetFocus;
      Abort;
    end else if not Self.FFactory.IsValideCNPJ(Self.FView.edtCNPJ.Text) then
    begin
      Application.MessageBox('CNPJ informado não é válido, verifique.', 'Atenção', MB_OK+MB_ICONERROR);
      Self.FView.edtCNPJ.SetFocus;
      Abort;
    end;
    Self.GetDadosEmpresa(Self.FView.edtCNPJ.Text);
  end;
end;

procedure TEmpresaController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

procedure TEmpresaController.FormShow(Sender: TObject);
var
  LEmpresas : TObjectList<TEmpresaModel>;
  aIndex: Integer;
begin
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
  LEmpresas := Self.FEmpresaDao.GetAll;
  try
    if LEmpresas.Count > 0 then
    begin
      Self.FView.FDMEmpresa.Open;
      Self.FView.FDMEmpresa.EmptyDataSet;
      for aIndex := 0 to Pred(LEmpresas.Count) do
      begin
        Self.FView.FDMEmpresa.Insert;
        Self.FView.FDMEmpresaCO_EMPRESA.AsInteger     := LEmpresas[aIndex].co_empresa;
        Self.FView.FDMEmpresaNM_RAZAO_SOCIAL.AsString := LEmpresas[aIndex].nm_razao_social;
        Self.FView.FDMEmpresaNU_CNPJ.AsString         := LEmpresas[aIndex].nu_cnpj;
        Self.FView.FDMEmpresaTX_EMAIL.AsString        := LEmpresas[aIndex].tx_email;
        Self.FView.FDMEmpresaDT_CADASTRO.AsDateTime   := LEmpresas[aIndex].dt_cadastro;

        if not Self.FFactory.DateTimeIsNull(LEmpresas[aIndex].dt_alteracao) then
          Self.FView.FDMEmpresaDT_ALTERACAO.AsDateTime  := LEmpresas[aIndex].dt_alteracao;
        Self.FView.FDMEmpresa.Post;
      end;
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    end else begin
      Self.FStatus := TStatus.dsInactive;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FView.FDMEmpresa.Close;
    end;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  finally
    FreeAndNil(LEmpresas);
  end;
end;

procedure TEmpresaController.GetDadosEmpresa(const AValue: String);
var
  LRestClint    : TRESTClient;
  LRestRequest  : TRESTRequest;
  LRestResponse : TRESTResponse;
  LJSONGeral,
  LJSONOEstabelecimento : TJsonObject;
begin
  LRestClint            := TRESTClient.Create(nil);
  LRestResponse         := TRESTResponse.Create(nil);
  LRestRequest          := TRESTRequest.Create(nil);
  LRestRequest.Response := LRestResponse;
  LRestRequest.Client   := LRestClint;
  LRestClint.BaseURL    := 'https://publica.cnpj.ws/cnpj/'+AValue;
  try
    LRestRequest.Execute;
    if LRestResponse.StatusCode = 200 then
    begin
      try
        LJSONGeral                     := TJSONObject(LRestResponse.JSONValue);
        Self.FView.edtRazaoSocial.Text := LJSONGeral.GetValue('razao_social').Value;
        LJSONOEstabelecimento          := TJSONObject(LJSONGeral.GetValue('estabelecimento'));
        Self.FView.edtEmail.Text       := LJSONOEstabelecimento.GetValue('email').Value;
      finally
        //Nada a ser destruido.
      end;
    end
    else
    begin
      Application.MessageBox('Dados desta empresa não disponível em www.cnpj.ws', 'Atenção', MB_OK+MB_ICONINFORMATION);
      Self.FView.edtRazaoSocial.SetFocus;
    end;
  finally
    FreeAndNil(LRestClint);
    FreeAndNil(LRestRequest);
    FreeAndNil(LRestResponse);
  end;
end;

procedure TEmpresaController.gridBaseDblClick(Sender: TObject);
begin
  if Self.FView.FDMEmpresa.RecordCount = 0 then
    Exit;
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabCadastro;
  Self.FView.edtCodigo.Text          := FormatFloat('000000', Self.FView.FDMEmpresaCO_EMPRESA.AsInteger);
  Self.FView.edtCNPJ.Text            := Self.FView.FDMEmpresaNU_CNPJ.AsString;
  Self.FView.edtRazaoSocial.Text     := Self.FView.FDMEmpresaNM_RAZAO_SOCIAL.AsString;
  Self.FView.edtEmail.Text           := Self.FView.FDMEmpresaTX_EMAIL.Text;
  if Self.FStatus = TStatus.dsEdit then
    Exit;
  Self.FStatus                       := TStatus.dsBrowse;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aDisable);
end;

end.
