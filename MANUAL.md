# TestAction プログラム 動作マニュアル

## 概要
このプログラムは、親フォーム(FormMain)と子フォーム(FormSub)の間で、イベントハンドラーを使用してデータを双方向で送受信する Delphi アプリケーションです。

---

## プログラム構成

### Main.pas（親フォーム）
- **ウィンドウ**: FormMain（メインウィンドウ）
  - Button1: FormSub を開くボタン
  - Edit1: ユーザーが入力するテキストボックス

### Sub.pas（子フォーム）
- **ウィンドウ**: FormSub（モーダルダイアログ）
  - Edit1: 表示用テキストボックス
  - Button1: FormMain の Edit1 の値を取得してこのEdit1に表示するボタン

---

## 動作フロー

### 1. プログラム起動時
```
↓
FormMain が生成される
↓
FormCreate イベント発生
  → ReportMemoryLeaksOnShutdown := True; が実行される
     （プログラム終了時にメモリリークを検出する機能を有効化）
```

### 2. ユーザーが FormMain の Button1 をクリック
```
↓
Button1Click イベント発生
  ↓
  FormSub := TFormSub.Create(Self); が実行される
     （FormSub の新しいインスタンスを作成）
     （Self = FormMain がオーナーとなる）
  ↓
  FormSub.Edit1.Text := '111'; が実行される
     （FormSub の Edit1 に初期値 "111" をセット）
  ↓
  FormSub.OnGetMainText := GetMainText; が実行される
     （イベントハンドラー登録）
     （FormMain の GetMainText メソッドをイベントハンドラーとして割り当て）
  ↓
  FormSub.ShowModal; が実行される
     （FormSub をモーダルダイアログとして表示）
     （ここで処理が FormSub に移る）
```

### 3. FormSub が表示された後
- FormSub の Edit1 には "111" が表示されている状態
- FormSub の Button1 をクリック待機

### 4. ユーザーが FormSub の Button1 をクリック
```
↓
Button1Click イベント発生
  ↓
  DoGetMainText メソッドが呼ばれる
     ↓
     if Assigned(FOnGetMainText) then チェック
        （イベントハンドラーが登録されているか確認）
     ↓
     Edit1.Text := FOnGetMainText(); が実行される
        （登録されたイベントハンドラー = FormMain.GetMainText を実行）
        ↓
        FormMain.GetMainText メソッドが実行される
           （FormMain の Edit1.Text の値を取得して返す）
        ↓
        戻り値が FormSub の Edit1.Text に代入される
```

### 5. FormSub を閉じる
```
↓
ユーザーが FormSub を閉じる
  （OK ボタン、キャンセルボタン、または閉じるボタンをクリック）
↓
ShowModal の処理が終了
  ↓
  FormSub.Free が実行される
     （FormSub が破棄される）
  ↓
  処理が FormMain に戻る
```

---

## 使用シーン例

### シーン: ユーザーが FormMain の Edit1 に "Hello World" と入力し、FormSub を開く場合

**ステップ 1: FormMain の Edit1 に "Hello World" を入力**
```
FormMain
├── Edit1: "Hello World" ← ユーザー入力
└── Button1: クリック
```

**ステップ 2: FormSub が開く**
```
FormSub が表示される（モーダルダイアログ）
├── Edit1: "111" ← 初期値
└── Button1: クリック待機
```

**ステップ 3: FormSub の Button1 をクリック**
```
Button1Click イベント
  ↓
DoGetMainText メソッド実行
  ↓
FOnGetMainText() を実行（= FormMain.GetMainText()）
  ↓
FormMain の Edit1.Text の値 "Hello World" を取得
  ↓
FormSub の Edit1.Text に "Hello World" を代入
```

**ステップ 4: 結果**
```
FormSub
├── Edit1: "Hello World" ← FormMain から取得した値に更新
└── Button1: クリック済み
```

---

## コードの重要な部分

### FormMain 側（イベントハンドラー提供側）

**GetMainText メソッド**
```objectpascal
function TFormMain.GetMainText: string;
begin
  Result := Edit1.Text;
end;
```
- 役割: FormMain の Edit1.Text の値を取得して返す
- 戻り値型: string
- パラメータ: なし

**Button1Click イベント**
```objectpascal
procedure TFormMain.Button1Click(Sender: TObject);
begin
  var FormSub := TFormSub.Create(Self);
  try
    FormSub.Edit1.Text := '111';
    FormSub.OnGetMainText := GetMainText;  // イベントハンドラーを割り当て
    FormSub.ShowModal;
  finally
    FormSub.Free;
  end;
end;
```
- try-finally ブロックで確実に FormSub のメモリを解放

### FormSub 側（イベントハンドラー呼び出し側）

**TGetMainTextEvent イベント型定義**
```objectpascal
type
  TGetMainTextEvent = function: string of object;
```
- 役割: "パラメータなし、string 型を返す、オブジェクトメソッド型" のイベントハンドラー型を定義

**DoGetMainText メソッド**
```objectpascal
procedure TFormSub.DoGetMainText;
begin
  if Assigned(FOnGetMainText) then
    Edit1.Text := FOnGetMainText();
end;
```
- 役割: 登録されたイベントハンドラーを呼び出し、戻り値を Edit1.Text に代入
- Assigned() チェック: イベントハンドラーが登録されているか確認

---

## メモリ管理

**FormSub のメモリ確保と解放**
```objectpascal
var FormSub := TFormSub.Create(Self);  // メモリ確保
try
  // FormSub を使用
finally
  FormSub.Free;  // メモリ解放
end;
```
- try-finally パターンにより、エラーが発生しても確実にメモリを解放
- FormMain が FormSub のオーナーなので、二重解放のリスクはない

---

## 設計パターン

このコードは以下の Delphi デザインパターンを採用しています：

1. **イベントハンドラーパターン**
   - FormSub が必要な機能を FormMain に要求する
   - 疎結合な設計（Sub.pas で Main.pas を import しない）

2. **コールバックパターン**
   - イベントハンドラーを動的に割り当てる
   - 実行時に動作を変更可能

3. **親子フォーム関係**
   - FormSub は FormMain をオーナーとして作成
   - FormSub はモーダルダイアログとして表示

---

## トラブルシューティング

### 問題: FormSub の Button1 をクリックしても Edit1 が更新されない
**原因**: FormSub.OnGetMainText が割り当てられていない可能性
**解決**: Main.pas の Button1Click で必ず `FormSub.OnGetMainText := GetMainText;` が実行されていることを確認

### 問題: コンパイルエラー「型に互換性がありません」
**原因**: イベント型定義とメソッドのシグネチャが一致していない
**解決**: TGetMainTextEvent と GetMainText のシグネチャを確認
- TGetMainTextEvent: `function: string of object`
- GetMainText: `function TFormMain.GetMainText: string`

### 問題: メモリリーク警告が表示される
**原因**: FormSub.Free が実行されていない可能性
**解決**: try-finally ブロックの配置と FormSub.Free の実行を確認

---

## 拡張例

### 複数のイベントハンドラーを追加する場合
```objectpascal
type
  TFormSub = class(TForm)
    // ...
  private
    FOnGetMainText: TGetMainTextEvent;
    FOnSetMainText: TSetMainTextEvent;  // 新規追加
```

### FormSub から FormMain にデータを戻す場合
```objectpascal
type
  TSetMainTextEvent = procedure(const AText: string) of object;

// FormSub 側で呼び出し
if Assigned(FOnSetMainText) then
  FOnSetMainText(Edit1.Text);
```

---

**作成日**: 2025年12月30日  
**バージョン**: 1.0
