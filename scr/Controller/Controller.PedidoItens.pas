unit Controller.PedidoItens;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Generics.Collections, Framework.Factory, Model.PedidoItens,
  View.PedidoItens, FireDAC.Comp.Client, FireDAC.Stan.Param, Dao.Conexao,
  Vcl.Dialogs, System.Variants, Data.DB, Dao.PedidoItens;

type
  TPedidoItensController = class
  private
    FFactory : TFactory;
    FView : TFrmPedidoItens;
    FStatus : TStatus;
    FPedidoItensModel : TPedidoItensModel;
    FPedidoItensDao : TPedidoItensDao;
    FNu_pedido : Integer;
    procedure btnFecharClick(Sender : TObject);
    procedure btnNovoClick(Sender : TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGravarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnApagarClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure gridBaseDblClick(Sender: TObject);
    procedure FDMPedido_ItensAfterScroll(DataSet: TDataSet);
    procedure FDQProdutosAfterScroll(DataSet: TDataSet);
    procedure edtQuantidadeExit(Sender: TObject);
    procedure edtCodigoProdutoExit(Sender: TObject);
  public
    constructor Create(nu_pedido : Integer);
    destructor Destroy; override;
  end;

implementation

{ TPedido_ItensController }

procedure TPedidoItensController.btnApagarClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma a exclusão deste Produto do Pedido?', 'Atenção', MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = IDYES then
  begin
    Self.FStatus := TStatus.dsDelete;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    if Self.FPedidoItensDao.Delete(Self.FView.FDMPedido_ItensCO_ITEM.AsInteger) then
    begin
      Self.FFactory.ClearControls(Self.FView);
      Self.FormShow(Sender);
    end
    else
      Self.FormShow(Sender);
  end;
end;

procedure TPedidoItensController.btnCancelarClick(Sender: TObject);
begin
  if Self.FStatus = TStatus.dsEdit then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FView.edtCodigoProduto.Text := FormatFloat('000000', Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger);
    Self.FView.comboProduto.KeyValue := Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger;
    Self.FView.edtQuantidade.Text    := FloatToStr(Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat);
    Self.FView.edtValorVenda.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency);
    Self.FView.edtValorTotal.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_TOTAL.AsCurrency);
    Self.FStatus := TStatus.dsBrowse;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end else if Self.FStatus = TStatus.dsInsert then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FStatus := TStatus.dsBrowse;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end;
end;

procedure TPedidoItensController.btnEditarClick(Sender: TObject);
begin
  Self.FStatus := TStatus.dsEdit;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtCodigoItem.Text    := FormatFloat('000000', Self.FView.FDMPedido_ItensCO_ITEM.AsInteger);
  Self.FView.edtCodigoProduto.Text := FormatFloat('000000', Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger);
  Self.FView.comboProduto.KeyValue := Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger;
  Self.FView.edtQuantidade.Text    := FloatToStr(Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat);
  Self.FView.edtValorVenda.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency);
  Self.FView.edtValorTotal.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_TOTAL.AsCurrency);
end;

procedure TPedidoItensController.btnFecharClick(Sender: TObject);
begin
  Self.FView.Close;
end;

procedure TPedidoItensController.btnGravarClick(Sender: TObject);
var
  aListError : TStringList;
begin
  aListError := TStringList.Create;
  aListError.Add('Lista de erro(s):');
  aListError.Add('');
  try
    if String(Self.FView.comboProduto.Text).IsEmpty then
      aListError.Add('Produto');
    if String(Self.FView.edtQuantidade.Text).IsEmpty then
      aListError.Add('Quantidade');

    if aListError.Count > 2 then
    begin
      aListError.Add('');
      aListError.Add('O(s) campo(s) listado(s) acima são de preenchimento obrigatório.');

      Application.MessageBox(PChar(aListError.Text), 'Atenção', MB_OK+MB_ICONERROR);
      Self.FView.comboProduto.SetFocus;
    end else
    begin
      if Self.FStatus = TStatus.dsEdit then
        Self.FPedidoItensModel.nu_pedido := Self.FView.FDMPedido_ItensNU_PEDIDO.AsInteger
      else
        Self.FPedidoItensModel.nu_pedido   := Self.FNu_pedido;
      Self.FPedidoItensModel.co_item       := StrToInt(Self.FView.edtCodigoItem.Text);
      Self.FPedidoItensModel.co_produto    := Self.FView.comboProduto.KeyValue;
      Self.FPedidoItensModel.nu_quantidade := StrToFloatDef(Self.FView.edtQuantidade.Text, 1);
      Self.FPedidoItensModel.vl_venda      := StrToCurr(String(Self.FView.edtValorVenda.Text).Replace('.', ''));

      if Self.FStatus = TStatus.dsInsert then
      begin

        Self.FPedidoItensDao.Insert(Self.FPedidoItensModel);

        if not Self.FView.FDMPedido_Itens.Active then
          Self.FView.FDMPedido_Itens.Open;

        Self.FView.FDMPedido_Itens.Insert;
        Self.FView.FDMPedido_ItensCO_ITEM.AsInteger     := Self.FPedidoItensModel.co_item;
        Self.FView.FDMPedido_ItensNU_PEDIDO.AsInteger   := Self.FPedidoItensModel.nu_pedido;
        Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger  := Self.FPedidoItensModel.co_produto;
        Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat := Self.FPedidoItensModel.nu_quantidade;
        Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency   := Self.FPedidoItensModel.vl_venda;
        Self.FView.FDMPedido_Itens.Post;

      end else if Self.FStatus = TStatus.dsEdit then
        Self.FPedidoItensDao.Update(Self.FPedidoItensModel);

      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    end;
  finally
    FreeAndNil(aListError);
  end;
end;

procedure TPedidoItensController.btnNovoClick(Sender: TObject);
begin
  Self.FFactory.ClearControls(Self.FView);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtCodigoItem.Text := FormatFloat('000000', Self.FPedidoItensDao.GetNewID('INC_CO_ITEM'));
  Self.FView.edtCodigoProduto.SetFocus;
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

constructor TPedidoItensController.Create(nu_pedido: Integer);
begin
  Self.FNu_pedido                        := nu_pedido;
  Self.FFactory                          := TFactory.Create;
  Self.FPedidoItensModel                 := TPedidoItensModel.Create;
  Self.FPedidoItensDao                   := TPedidoItensDao.Create;
  Self.FView                             := TFrmPedidoItens.Create(nil);
  Self.FView.KeyPreview                  := True;
  Self.FView.btnFechar.OnClick           := Self.btnFecharClick;
  Self.FView.btnNovo.OnClick             := Self.btnNovoClick;
  Self.FView.btnEditar.OnClick           := Self.btnEditarClick;
  Self.FView.btnGravar.OnClick           := Self.btnGravarClick;
  Self.FView.OnShow                      := Self.FormShow;
  Self.FView.OnKeyPress                  := Self.FormKeyPress;
  Self.FView.btnCancelar.OnClick         := Self.btnCancelarClick;
  Self.FView.btnApagar.OnClick           := Self.btnApagarClick;
  Self.FView.gridBase.OnDblClick         := Self.gridBaseDblClick;
  Self.FView.FDMPedido_Itens.AfterScroll := Self.FDMPedido_ItensAfterScroll;
  Self.FView.FDQProdutos.AfterScroll     := Self.FDQProdutosAfterScroll;
  Self.FView.edtQuantidade.OnExit        := Self.edtQuantidadeExit;
  Self.FView.edtCodigoProduto.OnExit     := Self.edtCodigoProdutoExit;
  Self.FView.btnNovo.ShowHint            := True;
  Self.FView.btnNovo.Hint                := 'Adicionar um novo registro';
  Self.FView.btnEditar.ShowHint          := True;
  Self.FView.btnEditar.Hint              := 'Editar o registro corrente';
  Self.FView.btnGravar.ShowHint          := True;
  Self.FView.btnGravar.Hint              := 'Gravar o registro corrente';
  Self.FView.btnCancelar.ShowHint        := True;
  Self.FView.btnCancelar.Hint            := 'Cnacelar a operação corrente';
  Self.FView.btnApagar.ShowHint          := True;
  Self.FView.btnApagar.Hint              := 'Apagar o registro corrente';
  Self.FView.btnFechar.ShowHint          := True;
  Self.FView.btnFechar.Hint              := 'Fechar';
  Self.FStatus                           := TStatus.dsInactive;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FView.ShowModal;
end;

destructor TPedidoItensController.Destroy;
begin
  FreeAndNil(Self.FPedidoItensDao);
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FPedidoItensModel);
  FreeAndNil(Self.FView);
  inherited;
end;

procedure TPedidoItensController.edtCodigoProdutoExit(Sender: TObject);
begin
  if (Self.FStatus = TStatus.dsEdit) or (Self.FStatus = TStatus.dsInsert) then
  begin
    if not String(Self.FView.edtCodigoProduto.Text).IsEmpty then
    begin
      if not Self.FView.FDQProdutos.Locate('CO_PRODUTO', Self.FView.edtCodigoProduto.Text, []) then
      begin
        Application.MessageBox('Produto não cadastro, verifique!', 'Atenção', MB_OK+MB_ICONWARNING);
        Self.FView.edtCodigoProduto.SelectAll;
        Self.FView.edtCodigoProduto.SetFocus;
        Exit;
      end
      else
      begin
        Self.FView.comboProduto.KeyValue := Self.FView.edtCodigoProduto.Text;
        Self.FView.edtQuantidade.SetFocus;
      end;
    end;
  end;
end;

procedure TPedidoItensController.edtQuantidadeExit(Sender: TObject);
var
  LQuantidade : Double;
  LValor : Currency;
begin
  if (Self.FStatus = TStatus.dsEdit) or (Self.FStatus = TStatus.dsInsert) then
  begin
    if not String(Self.FView.edtQuantidade.Text).IsEmpty then
    begin
      LQuantidade := StrToFloatDef(Self.FView.edtQuantidade.Text, 1);
      LValor := StrToCurr(String(Self.FView.edtValorVenda.Text).Replace('.', ''));
      Self.FView.edtValorTotal.Text := FormatFloat('#,##0.00', (LValor * LQuantidade));
    end;
  end;
end;

procedure TPedidoItensController.FDMPedido_ItensAfterScroll(DataSet: TDataSet);
begin
  if Self.FStatus <> TStatus.dsBrowse then
    Exit;
  Self.FView.edtCodigoProduto.Text := FormatFloat('000000', Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger);
  Self.FView.comboProduto.KeyValue := Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger;
  Self.FView.edtQuantidade.Text    := FloatToStr(Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat);
  Self.FView.edtValorVenda.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency);
  Self.FView.edtValorTotal.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_TOTAL.AsCurrency);
end;

procedure TPedidoItensController.FDQProdutosAfterScroll(DataSet: TDataSet);
begin
  if (not (String(Self.FView.comboProduto.Text).IsEmpty)) or (not (String(Self.FView.edtCodigoProduto.Text).IsEmpty))  then
  begin
    Self.FView.edtCodigoProduto.Text := FormatFloat('000000', DataSet.FieldByName('CO_PRODUTO').AsInteger);
    Self.FView.edtValorVenda.Text    := FormatFloat('#,##0.00', DataSet.FieldByName('VL_VENDA').AsCurrency);
  end;
end;

procedure TPedidoItensController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

procedure TPedidoItensController.FormShow(Sender: TObject);
var
  LPedido_Itens : TObjectList<TPedidoItensModel>;
  aIndex: Integer;
begin
  Self.FView.Caption := 'Cadastro de Itens do Pedido nº ' + FormatFloat('000000', Self.FNu_pedido);
  LPedido_Itens := Self.FPedidoItensDao.GetAll(Self.FNu_pedido);
  try
    if LPedido_Itens.Count > 0 then
    begin
      Self.FView.FDMPedido_Itens.Open;
      Self.FView.FDMPedido_Itens.EmptyDataSet;
      for aIndex := 0 to Pred(LPedido_Itens.Count) do
      begin
        Self.FView.FDMPedido_Itens.Insert;
        Self.FView.FDMPedido_ItensCO_ITEM.AsInteger      := LPedido_Itens[aIndex].co_item;
        Self.FView.FDMPedido_ItensNU_PEDIDO.AsInteger    := LPedido_Itens[aIndex].nu_pedido;
        Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger   := LPedido_Itens[aIndex].co_produto;
        Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat  := LPedido_Itens[aIndex].nu_quantidade;
        Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency    := LPedido_Itens[aIndex].vl_venda;
        Self.FView.FDMPedido_ItensVL_TOTAL.AsCurrency    := LPedido_Itens[aIndex].vl_total;
        Self.FView.FDMPedido_ItensDT_CADASTRO.AsDateTime := LPedido_Itens[aIndex].dt_cadastro;
        Self.FView.FDMPedido_Itens.Post;
      end;
      Self.FView.FDMPedido_Itens.First;
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    end else begin
      Self.FStatus := TStatus.dsInactive;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FView.FDMPedido_Itens.Close;
    end;
    Self.FPedidoItensDao.ToLoadComboProduto(Self.FView.FDQProdutos);
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  finally
    FreeAndNil(LPedido_Itens);
  end;
end;

procedure TPedidoItensController.gridBaseDblClick(Sender: TObject);
begin
  Self.btnEditarClick(Sender);
end;

end.
