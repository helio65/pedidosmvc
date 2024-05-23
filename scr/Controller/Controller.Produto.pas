unit Controller.Produto;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Generics.Collections, FireDAC.Comp.Client, FireDAC.Stan.Param,
  Vcl.Dialogs, System.Variants, Controller.Imagens, Framework.Factory,
  Dao.Conexao, Model.Produto, View.Produto, Dao.Produto;

type
  TProdutoController = class
  private
    FView : TFrmProduto;
    FFactory : TFactory;
    FStatus : TStatus;
    FProdutoModel : TProdutoModel;
    FProdutoDao : TProdutoDao;
    FImagensController : TImagensController;
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
    procedure edtValorKeyPress(Sender: TObject; var Key: Char);
    procedure edtValorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnImagensOnClick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProdutoController }

procedure TProdutoController.btnApagarClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma a exclusão deste Produto?', 'Atenção', MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = IDYES then
  begin
    Self.FStatus := TStatus.dsDelete;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    if Self.FProdutoDao.Delete(Self.FView.FDMProdutoCO_PRODUTO.AsInteger) then
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

procedure TProdutoController.btnCancelarClick(Sender: TObject);
begin
  if Self.FStatus = TStatus.dsEdit then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FView.edtCodigo.Text      := FormatFloat('000000', Self.FView.FDMProdutoCO_PRODUTO.AsInteger);
    Self.FView.edtDescricao.Text   := Self.FView.FDMProdutoTX_DESCRICAO.AsString;
    Self.FView.edtValor.Text       := FormatFloat('#,##0.00', Self.FView.FDMProdutoVL_VENDA.AsCurrency);
    Self.FView.edtEstoque.Text     := FloatToStr(Self.FView.FDMProdutoNU_ESTOQUE.AsFloat);
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

procedure TProdutoController.btnEditarClick(Sender: TObject);
begin
  Self.FStatus := TStatus.dsEdit;
  if Self.FView.pgcPrincipal.ActivePage = Self.FView.tabConsulta then
    Self.gridBaseDblClick(Sender);
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
end;

procedure TProdutoController.btnFecharClick(Sender: TObject);
begin
  Self.FView.Close;
end;

procedure TProdutoController.btnGravarClick(Sender: TObject);
var
  aListError : TStringList;
begin
  aListError := TStringList.Create;
  aListError.Add('Lista de erro(s):');
  aListError.Add('');
  try
    if String(Self.FView.edtDescricao.Text).IsEmpty then
      aListError.Add('Descrição');
    if String(Self.FView.edtValor.Text).IsEmpty then
      aListError.Add('Valor');

    if aListError.Count > 2 then
    begin
      aListError.Add('');
      aListError.Add('O(s) campo(s) listado(s) acima são de preenchimento obrigatório.');

      Application.MessageBox(PChar(aListError.Text), 'Atenção', MB_OK+MB_ICONERROR);
      Self.FView.edtDescricao.SetFocus;
    end else
    begin
      if Self.FStatus = TStatus.dsEdit then
      begin
        Self.FProdutoModel.co_produto := Self.FView.FDMProdutoCO_PRODUTO.AsInteger;
        Self.FProdutoModel.dt_alteracao := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
      end
      else
        Self.FProdutoModel.co_produto := StrToInt(Self.FView.edtCodigo.Text);
      Self.FProdutoModel.tx_descricao := Self.FView.edtDescricao.Text;
      Self.FProdutoModel.vl_venda     := StrToCurr(String(Self.FView.edtValor.Text).Replace('.', ''));
      Self.FProdutoModel.nu_estoque   := StrToFloatDef(Self.FView.edtEstoque.Text, 0);

      if Self.FStatus = TStatus.dsInsert then
      begin

        Self.FProdutoDao.Insert(Self.FProdutoModel);

        if not Self.FView.FDMProduto.Active then
          Self.FView.FDMProduto.Open;

        Self.FView.FDMProduto.Insert;
        Self.FView.FDMProdutoCO_PRODUTO.AsInteger  := StrtoInt(Self.FView.edtCodigo.Text);
        Self.FView.FDMProdutoTX_DESCRICAO.AsString := Self.FView.edtDescricao.Text;
        Self.FView.FDMProdutoVL_VENDA.AsCurrency   := StrToCurr(String(Self.FView.edtValor.Text).Replace('.', ''));
        Self.FView.FDMProdutoNU_ESTOQUE.AsFloat    := StrToFloatDef(Self.FView.edtEstoque.Text, 0);
        Self.FView.FDMProduto.Post;

      end else if Self.FStatus = TStatus.dsEdit then
        Self.FProdutoDao.Update(Self.FProdutoModel);

      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    end;
  finally
    FreeAndNil(aListError);
  end;
end;

procedure TProdutoController.btnImagensOnClick(Sender: TObject);
begin
  if Self.FView.FDMProduto.RecordCount = 0 then
    Exit;
  try
    Self.FImagensController := TImagensController.Create(Self.FView.FDMProdutoCO_PRODUTO.AsInteger);
  finally
    FreeAndNil(Self.FImagensController);
  end;
end;

procedure TProdutoController.btnNovoClick(Sender: TObject);
begin
  Self.FFactory.ClearControls(Self.FView);
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabCadastro;
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtDescricao.SetFocus;
  Self.FView.edtCodigo.Text := FormatFloat('000000', Self.FProdutoDao.GetNewID('INC_CO_PRODUTO'));
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

procedure TProdutoController.btnVoltarClick(Sender: TObject);
begin
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
end;

constructor TProdutoController.Create;
begin
  Self.FFactory                   := TFactory.Create;
  Self.FProdutoModel              := TProdutoModel.Create;
  Self.FProdutoDao                := TProdutoDao.Create;
  Self.FView                      := TFrmProduto.Create(nil);
  Self.FView.KeyPreview           := True;
  Self.FView.btnFechar.OnClick    := Self.btnFecharClick;
  Self.FView.btnNovo.OnClick      := Self.btnNovoClick;
  Self.FView.btnEditar.OnClick    := Self.btnEditarClick;
  Self.FView.btnGravar.OnClick    := Self.btnGravarClick;
  Self.FView.btnVoltar.OnClick    := Self.btnVoltarClick;
  Self.FView.btnImagens.OnClick   := Self.btnImagensOnClick;
  Self.FView.OnShow               := Self.FormShow;
  Self.FView.OnKeyPress           := Self.FormKeyPress;
  Self.FView.btnCancelar.OnClick  := Self.btnCancelarClick;
  Self.FView.btnApagar.OnClick    := Self.btnApagarClick;
  Self.FView.gridBase.OnDblClick  := Self.gridBaseDblClick;
  Self.FView.edtValor.OnKeyPress  := Self.edtValorKeyPress;
  Self.FView.edtValor.OnKeyUp     := Self.edtValorKeyUp;
  Self.FView.btnNovo.ShowHint     := True;
  Self.FView.btnNovo.Hint         := 'Adicionar um novo registro';
  Self.FView.btnEditar.ShowHint   := True;
  Self.FView.btnEditar.Hint       := 'Editar o registro corrente';
  Self.FView.btnGravar.ShowHint   := True;
  Self.FView.btnGravar.Hint       := 'Gravar o registro corrente';
  Self.FView.btnCancelar.ShowHint := True;
  Self.FView.btnCancelar.Hint     := 'Cnacelar a operação corrente';
  Self.FView.btnApagar.ShowHint   := True;
  Self.FView.btnApagar.Hint       := 'Apagar o registro corrente';
  Self.FView.btnFechar.ShowHint   := True;
  Self.FView.btnFechar.Hint       := 'Fechar';
  Self.FFactory.ControlControls(FView, TStatus.dsInactive);
  Self.FView.ShowModal;
end;

destructor TProdutoController.Destroy;
begin
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FProdutoModel);
  FreeAndNil(Self.FProdutoDao);
  FreeAndNil(Self.FView);
  inherited;
end;

procedure TProdutoController.edtValorKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['0'..'9', #8, ^V, ^C, ^X]) then
    Key := #0;
end;

procedure TProdutoController.edtValorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Self.FFactory.FormatCurrecyValue(Sender, Key, Shift);
end;

procedure TProdutoController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

procedure TProdutoController.FormShow(Sender: TObject);
var
  LProdutos : TObjectList<TProdutoModel>;
  aIndex: Integer;
begin
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
  LProdutos := Self.FProdutoDao.GetAll;
  try
    if LProdutos.Count > 0 then
    begin
      Self.FView.FDMProduto.Open;
      Self.FView.FDMProduto.EmptyDataSet;
      for aIndex := 0 to Pred(LProdutos.Count) do
      begin
        Self.FView.FDMProduto.Insert;
        Self.FView.FDMProdutoCO_PRODUTO.AsInteger   := LProdutos[aIndex].co_produto;
        Self.FView.FDMProdutoTX_DESCRICAO.AsString  := LProdutos[aIndex].tx_descricao;
        Self.FView.FDMProdutoVL_VENDA.AsCurrency    := LProdutos[aIndex].vl_venda;
        Self.FView.FDMProdutoNU_ESTOQUE.AsFloat     := LProdutos[aIndex].nu_estoque;
        Self.FView.FDMProdutoDT_CADASTRO.AsDateTime := LProdutos[aIndex].dt_cadastro;

        if not Self.FFactory.DateTimeIsNull(LProdutos[aIndex].dt_alteracao) then
          Self.FView.FDMProdutoDT_ALTERACAO.AsDateTime  := LProdutos[aIndex].dt_alteracao;
        Self.FView.FDMProduto.Post;
      end;
      Self.FView.FDMProduto.First;
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    end else begin
      Self.FStatus := TStatus.dsInactive;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FView.FDMProduto.Close;
    end;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  finally
    FreeAndNil(LProdutos);
  end;
end;

procedure TProdutoController.gridBaseDblClick(Sender: TObject);
begin
  if Self.FView.FDMProduto.RecordCount = 0 then
    Exit;
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabCadastro;
  Self.FView.edtCodigo.Text          := FormatFloat('000000', Self.FView.FDMProdutoCO_PRODUTO.AsInteger);
  Self.FView.edtDescricao.Text       := Self.FView.FDMProdutoTX_DESCRICAO.AsString;
  Self.FView.edtValor.Text           := FormatFloat('#,##0.00', Self.FView.FDMProdutoVL_VENDA.AsCurrency);
  Self.FView.edtEstoque.Text         := FloatToStr(Self.FView.FDMProdutoNU_ESTOQUE.AsFloat);
  if Self.FStatus = TStatus.dsEdit then
    Exit;
  Self.FStatus                       := TStatus.dsBrowse;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aDisable);
end;

end.
