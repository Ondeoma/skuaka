package skuaka.shared

import io.circe.*

import java.util.UUID

// バックエンド(JVM)と画面(Scala.js)の両方で使う API の型。
// 時刻のある予定の日時は ISO 8601 の UTC の文字列（2026-11-15T01:00:00Z）、終日の予定の日付は "yyyy-MM-dd"（終わりの日を含む）。
// 画面(Scala.js)で java.time を使わないため、どちらも文字列で持つ。

final case class ErrorDto(error: String) derives Encoder.AsObject, Decoder

/**
 * ログインや連携の後に戻る先. 同じサイトのパスだけを通す（サーバーと画面で同じ確認をする）.
 * 「//evil」「/\\evil」や、ブラウザが読み飛ばすタブ・改行を挟んだ「/\t/evil」で別のサイトへ飛ばされないよう、使える文字を絞る.
 */
object SafePath {
  private val re = "^/(?![/\\\\])[A-Za-z0-9\\-._~!$&'()*+,;=:@%/?#]*$".r
  def of(s: Option[String], default: String = "/"): String = s.map(_.trim).filter(re.matches).getOrElse(default)
}

/** skakaUrl は Web 会議（skaka）と連携できるときの skaka の URL. */
final case class ClientConfigDto(devLogin: Boolean, oidc: Boolean, version: String, skakaUrl: Option[String]) derives Encoder.AsObject, Decoder

/** 自分が skaka（Web 会議）と連携しているか. */
final case class SkakaStatusDto(enabled: Boolean, connected: Boolean, url: Option[String]) derives Encoder.AsObject, Decoder

/**
 * 予定の場所に入れる Web 会議（skaka）の会議室の URL（{skaka の URL}/r/{コード}）.
 * 場所が空なら URL だけ、あれば「{場所} / {URL}」（skaka が予定に会議室を付けるときと同じ書き方）.
 */
object Meeting {
  private def pattern(base: String) = (java.util.regex.Pattern.quote(base.stripSuffix("/") + "/r/") + "([a-z0-9-]+)").r

  /** 場所の中の会議室の URL とコード. */
  def find(location: String, base: String): Option[(String, String)] =
    pattern(base).findFirstMatchIn(location).map(m => (m.matched, m.group(1)))

  def append(location: String, url: String): String = if (location.trim.isEmpty) url else s"${location.trim} / $url"
}

final case class UserBrief(id: UUID, name: String, email: Option[String], avatarUrl: Option[String]) derives Encoder.AsObject, Decoder

/** avatarUploaded は、自分で保存したアイコンがあるか（なければ avatarUrl は IdP の画像）. */
final case class MeDto(id: UUID, name: String, email: Option[String], avatarUrl: Option[String], timezone: String, avatarUploaded: Boolean)
    derives Encoder.AsObject, Decoder {
  def brief: UserBrief = UserBrief(id, name, email, avatarUrl)
}

/** 保存できるアイコン（画像の種類と大きさの上限）. SVG はスクリプトを含められるので受け付けない. */
object AvatarImage {
  val MaxBytes = 1024 * 1024
  /** 先頭のバイトから画像の種類を決める（PNG・JPEG・GIF・WebP 以外は None）. */
  def detect(b: Array[Byte]): Option[String] = {
    def at(i: Int, xs: Int*): Boolean = b.length >= i + xs.size && xs.zipWithIndex.forall((x, j) => (b(i + j) & 0xff) == x)
    if (at(0, 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)) Some("image/png")
    else if (at(0, 0xff, 0xd8, 0xff)) Some("image/jpeg")
    else if (at(0, 0x47, 0x49, 0x46, 0x38)) Some("image/gif")
    else if (at(0, 0x52, 0x49, 0x46, 0x46) && at(8, 0x57, 0x45, 0x42, 0x50)) Some("image/webp")
    else None
  }
}

final case class UpdateMeReq(name: Option[String], timezone: Option[String]) derives Encoder.AsObject, Decoder

/** カレンダー・予定の色の候補（値 → 名前）. 新しいカレンダーは Default. */
object Palette {
  val colors: List[(String, String)] = List(
    "#d50000" -> "赤", "#e67c73" -> "珊瑚", "#f4511e" -> "橙", "#f6bf26" -> "黄", "#33b679" -> "若葉", "#0b8043" -> "緑",
    "#039be5" -> "空", "#3f51b5" -> "藍", "#7986cb" -> "藤", "#8e24aa" -> "紫", "#616161" -> "灰")
  val Default = "#039be5"
}

/**
 * カレンダーの権限. owner は所有者（共有・削除）、edit は予定の編集、view は閲覧だけ、
 * busy は予定のある時間だけ（題名・場所・説明・ゲストは見えない. チームへの公開でだけ使う）.
 */
object CalRole {
  val Busy = "busy"
  val View = "view"
  val Edit = "edit"
  val Owner = "owner"
  val all: List[String] = List(Busy, View, Edit, Owner)
  def rank(r: String): Int = r match {
    case Busy  => 1
    case View  => 2
    case Edit  => 3
    case Owner => 4
    case _     => 0
  }
  def label(r: String): String = r match {
    case Busy  => "予定あり・なしのみ"
    case View  => "閲覧のみ"
    case Edit  => "予定の変更"
    case Owner => "所有者"
    case _     => r
  }
}

/** 招待への返事. */
object Rsvp {
  val NeedsAction = "needs_action"
  val Accepted = "accepted"
  val Declined = "declined"
  val Tentative = "tentative"
  val all: List[String] = List(NeedsAction, Accepted, Declined, Tentative)
  def label(r: String): String = r match {
    case NeedsAction => "未回答"
    case Accepted    => "参加"
    case Declined    => "不参加"
    case Tentative   => "未定"
    case _           => r
  }
}

/** チームへの公開の範囲（none: しない / busy: 予定あり・なしのみ / view: 予定の詳細）. */
object TeamAccess {
  val None = "none"
  val Busy = CalRole.Busy
  val View = CalRole.View
  val all: List[String] = List(None, Busy, View)
  def label(a: String): String = a match {
    case Busy => "予定あり・なしのみ"
    case View => "予定の詳細"
    case _    => "公開しない"
  }
  /** 予定あり・なしのみの予定の題名. */
  val BusyTitle = "予定あり"
}

/**
 * カレンダー. color はカレンダーの色、myColor は自分だけの表示の色、visible は自分の画面に出すか.
 * publicLink はログインしない人への公開リンク（所有者だけに返す. 公開していなければ None）.
 * teamAccess はチームへの公開の範囲、viaTeam はチームへの公開から自分で加えたもの（共有されたのではない）.
 */
final case class CalendarDto(id: UUID, name: String, description: String, color: String, myColor: Option[String], visible: Boolean,
                             role: String, owner: UserBrief, primary: Boolean, publicLink: Option[PublicLinkDto], teamAccess: String,
                             viaTeam: Boolean) derives Encoder.AsObject, Decoder {
  def displayColor: String = myColor.getOrElse(color)
}

/**
 * ログインしない人への公開リンク. pageUrl は閲覧のページ、icsUrl は iCalendar の配信（どちらも同じ秘密の値）.
 * access は busy（予定あり・なしのみ）か view（予定の詳細）.
 */
final case class PublicLinkDto(pageUrl: String, icsUrl: String, access: String) derives Encoder.AsObject, Decoder

/** 公開リンクを作る・公開の範囲を変える. regenerate なら秘密の値を作り直す（前のリンクは使えなくなる）. */
final case class PublicLinkReq(access: String, regenerate: Boolean = false) derives Encoder.AsObject, Decoder

/** 公開リンクで見るカレンダー（ログインしない人向け. 持ち主のメールアドレスは含めない）. */
final case class PublicCalendarDto(id: UUID, name: String, description: String, color: String, owner: UserBrief, access: String)
    derives Encoder.AsObject, Decoder

/** teamAccess を省くと、作成では公開しない、変更では今のまま. */
final case class CalendarReq(name: String, color: String, description: String, teamAccess: Option[String] = None)
    derives Encoder.AsObject, Decoder

/** チームの人が公開しているカレンダー. added は自分の一覧に入っているか（共有されているものも含む）. */
final case class TeamCalendarDto(id: UUID, name: String, description: String, color: String, owner: UserBrief, primary: Boolean,
                                 access: String, added: Boolean) derives Encoder.AsObject, Decoder

final case class CalendarPrefReq(color: Option[String], visible: Option[Boolean]) derives Encoder.AsObject, Decoder

final case class MemberDto(user: UserBrief, role: String) derives Encoder.AsObject, Decoder

final case class MemberReq(user: String, role: String) derives Encoder.AsObject, Decoder

final case class AttendeeDto(user: UserBrief, response: String) derives Encoder.AsObject, Decoder

/** 通知（予定の何分前に知らせるか）. 1つの予定に MaxCount 個まで、4週間前まで. */
object Reminders {
  val MaxCount = 5
  val MaxMinutes = 40320
  /** 重複を除いて、近い順に. */
  def normalize(ms: List[Int]): List[Int] = ms.distinct.sorted
  def label(m: Int): String =
    if (m == 0) "予定の時刻"
    else if (m % 10080 == 0) s"${m / 10080}週間前"
    else if (m % 1440 == 0) s"${m / 1440}日前"
    else if (m % 60 == 0) s"${m / 60}時間前"
    else s"${m}分前"
  def text(ms: List[Int]): String = if (ms.isEmpty) "なし" else ms.map(label).mkString("・")
}

/**
 * 予定の作成・変更. start / end は終日なら日付、そうでなければ UTC の日時. timezone は繰り返しを展開する基準のタイムゾーン.
 * reminders は通知（何分前の一覧. 空なら通知しない）. withMeeting なら、保存したあとで Web 会議（skaka）の会議室を作り、場所に URL を入れる.
 */
final case class EventReq(calendarId: UUID, title: String, description: String, location: String, allDay: Boolean,
                          start: String, end: String, timezone: String, color: Option[String], recurrence: Option[Recurrence],
                          attendees: List[UUID], reminders: List[Int], version: Option[Int], withMeeting: Option[Boolean] = None)
    derives Encoder.AsObject, Decoder

/** 予定（繰り返しの元の定義）. meetingError は、Web 会議の会議室を作れなかったとき（予定は保存してある）. */
final case class EventDto(id: UUID, calendarId: UUID, title: String, description: String, location: String, allDay: Boolean,
                          start: String, end: String, timezone: String, color: Option[String], recurrence: Option[Recurrence],
                          attendees: List[AttendeeDto], reminders: List[Int], organizer: UserBrief, canEdit: Boolean,
                          myResponse: Option[String], version: Int, busy: Boolean = false, meetingError: Option[String] = None)
    derives Encoder.AsObject, Decoder

/**
 * 画面に並べる1回分の予定（繰り返しを展開したもの）.
 * occurrence はこの回の元の開始（繰り返しの「この予定だけ」の変更・削除で使う）.
 * invited は、カレンダーを見られないが招待されているので見える予定.
 * busy は、予定あり・なしだけ見えるカレンダーの予定（題名などは伏せてある）.
 */
final case class OccurrenceDto(eventId: UUID, calendarId: UUID, title: String, location: String, allDay: Boolean, start: String, end: String,
                               color: Option[String], recurring: Boolean, occurrence: String, canEdit: Boolean, invited: Boolean,
                               myResponse: Option[String], reminders: List[Int], guests: Int, busy: Boolean = false) derives Encoder.AsObject, Decoder

final case class RsvpReq(response: String) derives Encoder.AsObject, Decoder

/** 繰り返しの変更・削除の範囲. */
object EditScope {
  val All = "all"
  val This = "this"
  val Following = "following"
}

/** API トークン. scope は read（参照だけ）か write（変更も）. 日時は UTC の ISO 8601. */
object TokenScope {
  val Read = "read"
  val Write = "write"
  val all: List[String] = List(Read, Write)
  def label(s: String): String = if (s == Write) "読み書き" else "参照のみ"
}

final case class ApiTokenDto(id: UUID, name: String, prefix: String, scope: String, createdAt: String, expiresAt: Option[String],
                             lastUsedAt: Option[String]) derives Encoder.AsObject, Decoder

final case class ApiTokenReq(name: String, scope: String, expiresInDays: Option[Int]) derives Encoder.AsObject, Decoder

/** 作ったときだけ token（秘密の値）を返す. */
final case class ApiTokenCreated(info: ApiTokenDto, token: String) derives Encoder.AsObject, Decoder
