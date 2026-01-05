# 🚀 **SubmitForm関数 とは？**

```
SubmitForm(FormName)
```

**EditForm（編集フォーム）に入力された内容をデータソースに保存する関数**です。

- 新規モード → レコードを追加  
- 編集モード → レコードを更新  
- ビュー（表示）モード → 保存不可  

Power Apps のフォームは「フォームコントロールがデータソースと自動でバインドされる」ため、SubmitForm を呼ぶだけで保存処理が完結します。

---

# 🧩 **SubmitForm の基本動作**

## ✔ 1. フォームの入力値を検証（Validation）
必須項目やデータ型のチェックが自動で行われます。

## ✔ 2. データソースへ保存（Create / Update）
- 新規 → Defaults(DataSource) を元に新規レコード作成  
- 編集 → 既存レコードを更新  

## ✔ 3. 保存成功時
- **OnSuccess** が実行される  
- **FormName.LastSubmit** に保存されたレコードが入る  
- フォームは **ViewMode** に戻る（デフォルト）

## ✔ 4. 保存失敗時
- **OnFailure** が実行される  
- Error メッセージが Form.Error に格納される  

---

# 🎯 **SubmitForm のプロパティ連動（重要）**

フォームには以下のプロパティがあります：

| プロパティ | 説明 |
|-----------|------|
| **Mode** | New / Edit / View |
| **Valid** | 入力が有効か |
| **Unsaved** | 未保存の変更があるか |
| **Error** | 保存失敗時のエラーメッセージ |
| **LastSubmit** | 最後に保存されたレコード |

SubmitForm はこれらと密接に連動します。

---

# 🧪 **実務でよく使う SubmitForm パターン（明浩さん向け）**

---

## ✔ 1. 保存後に確認画面へ遷移
```
SubmitForm(Form1)
```

Form1 の **OnSuccess** に：

```
Navigate(ConfirmScreen, { rec: Form1.LastSubmit })
```

---

## ✔ 2. 保存成功時に通知
Form1.OnSuccess:

```
Notify($"保存しました（ID: {Form1.LastSubmit.ID}）", NotificationType.Success)
```

---

## ✔ 3. 保存失敗時のエラー表示
Form1.OnFailure:

```
Notify($"保存に失敗しました: {Form1.Error}", NotificationType.Error)
```

---

## ✔ 4. 保存後に関連テーブルへレコード追加
Form1.OnSuccess:

```
Patch(
    RelatedTable,
    Defaults(RelatedTable),
    {
        ParentID: Form1.LastSubmit.ID,
        Comment: txtComment.Text
    }
)
```

---

## ✔ 5. 保存後にフォームをリセットして新規入力へ
Form1.OnSuccess:

```
ResetForm(Form1);
NewForm(Form1)
```

---

# 🔍 **SubmitForm と Patch の違い（実務で重要）**

| 項目 | SubmitForm | Patch |
|------|------------|--------|
| 保存対象 | フォームの入力値 | 任意の値 |
| バリデーション | 自動 | 手動 |
| 添付ファイル | 自動対応 | 手動で構築 |
| LastSubmit | 使える | 使えない（Patch の戻り値を使う） |
| 複雑なロジック | 不向き | 向いている |

あなたのように「安全で再現性のある保存」を重視するなら、  
**フォームで完結する場面は SubmitForm が最適**です。

---

# ⚠️ **SubmitForm の注意点（重要）**

### ❗ 1. Patch と違い、保存前に値を加工しにくい  
→ 加工したい場合は **Update** プロパティを使う

### ❗ 2. OnSuccess / OnFailure を必ず設定する  
→ エラー処理がないとユーザー体験が悪化する

### ❗ 3. フォームの Mode によって動作が変わる  
→ NewForm / EditForm の切り替えを正しく行う

### ❗ 4. 保存前に LastSubmit を参照しても古い値  
→ 保存後に参照すること

---

# 🧭 **SubmitForm を使うべき場面（明浩さん向けの指針）**

### ✔ 使うべき
- フォームで入力 → そのまま保存  
- バリデーションを自動化したい  
- 添付ファイルを扱う  
- 保存後のレコードを LastSubmit で扱いたい  

### ✔ Patch を使うべき
- 保存前に値を加工したい  
- 複数テーブルを同時に更新したい  
- 条件分岐が複雑  
- フォームを使わない UI の場合  

---

# 📘 必要なら…

- SubmitForm / Patch の完全比較チャート  
- あなたのアプリの保存ロジックを最適化する設計例  
- OnSuccess / OnFailure のベストプラクティス集  

こういった資料も作れます。
