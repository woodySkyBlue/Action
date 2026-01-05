# 🚀 **LastSubmitプロパティ とは？**

**フォーム（EditForm）で SubmitForm() を実行した直後に、  
データソースに保存された “最後に作成または更新されたレコード” を返すプロパティ**です。

```
FormName.LastSubmit
```

- **新規作成時** → 追加されたレコード  
- **編集時** → 更新されたレコード  
- **SubmitForm() が成功した直後のみ有効**

---

# 🎯 **LastSubmit が役立つ理由**

Power Apps では、SubmitForm() の後に「保存されたレコードの ID を知りたい」「保存結果を別画面に渡したい」という場面が多いですよね。

LastSubmit はまさにそのために存在します。

- 保存されたレコードの ID を取得  
- 保存後に別画面へ遷移  
- 保存した内容を確認画面に表示  
- 添付ファイルや関連データの登録に使う  

あなたのように「ロジックを明確にしたい」タイプには非常に使いやすい仕組みです。

---

# 🧩 **基本的な使い方**

## ■ 保存後に ID を取得
```
Form1.LastSubmit.ID
```

## ■ 保存後に確認画面へ遷移
```
Navigate(ConfirmScreen, { rec: Form1.LastSubmit })
```

## ■ 保存したレコードをギャラリーに反映
```
Collect(colSaved, Form1.LastSubmit)
```

---

# 🧪 **実務でよく使うパターン（明浩さん向け）**

## ✔ 1. 保存後にトースト通知
```
Notify($"保存しました（ID: {Form1.LastSubmit.ID}）", NotificationType.Success)
```

## ✔ 2. 保存後に別画面へ遷移し、保存内容を表示
```
Navigate(
    DetailScreen,
    ScreenTransition.Fade,
    { SelectedRecord: Form1.LastSubmit }
)
```

## ✔ 3. 保存後に関連テーブルへレコード追加
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

## ✔ 4. 添付ファイルを保存したレコードに紐づける
```
Patch(
    AttachmentsTable,
    Defaults(AttachmentsTable),
    {
        ParentID: Form1.LastSubmit.ID,
        File: UploadedFile
    }
)
```

---

# 🔍 **LastSubmit の中身（返されるレコード）**

データソースの構造に応じて、保存されたレコードのフィールドがすべて入っています。

例：SharePoint リストの場合  
```
{
    ID: 123,
    Title: "テスト",
    Created: 2026-01-06,
    Modified: 2026-01-06,
    Author: ...,
    Editor: ...
}
```

---

# ⚠️ **LastSubmit の注意点（重要）**

あなたのようにロジックの正確性を重視する方には、ここが特に大事です。

### ❗ 1. SubmitForm() が成功した直後でないと値が古い
画面を開いた直後などでは **前回の保存結果が残っている**ことがあります。

### ❗ 2. 複数フォームを使う場合はフォーム名を間違えない
```
Form1.LastSubmit
Form2.LastSubmit
```
は別物。

### ❗ 3. Patch() では LastSubmit は使えない
Patch の場合は **Patch の戻り値**を使います。

```
Set(varRec, Patch(DataSource, Defaults(DataSource), {...}))
varRec.ID
```

### ❗ 4. SubmitForm() の前に LastSubmit を参照しても意味がない
保存前は古い値のまま。

---

# 🧭 **LastSubmit を使うべき場面（明浩さん向けの指針）**

あなたの開発スタイルに合わせると：

### ✔ 使うべき場面
- フォーム保存後のレコードを確実に取得したい  
- 保存後の画面遷移  
- 保存結果の確認画面  
- 関連テーブルへの追加  
- 添付ファイルの紐づけ  

### ✔ 避けるべき場面
- Patch を使う場合（Patch の戻り値を使う方が安全）  
- 保存前に値を参照したい場合  

---

# 📘 必要なら…

- LastSubmit を使った「保存 → 確認画面」テンプレート  
- Patch と LastSubmit の使い分けガイド  
- あなたのアプリの保存ロジックを最適化する設計例  

こういった資料も作れます。
