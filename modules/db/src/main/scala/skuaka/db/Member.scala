package skuaka.db

import doobie.*

/**
 * calendar_member の role = 'team' は「チームに公開されたカレンダーを自分で加えた」印で、権限は calendar.team_access に従う.
 * 所有者が公開をやめたら（none）、その行は何も見せない. 別名は calendar が c、calendar_member が m.
 */
object Member {
  val Team = "team"

  /** 実際の権限（公開をやめたチームの行は null）. */
  def role(c: String, m: String): Fragment =
    Fragment.const(s"(case when $m.role = 'team' then nullif($c.team_access, 'none') else $m.role end)")

  /** 権限のある行か. */
  def active(c: String, m: String): Fragment = Fragment.const(s"($m.role <> 'team' or $c.team_access <> 'none')")

  /** 予定に対する権限（購読したカレンダーの予定は、所有者でも閲覧だけ）. */
  def eventRole(c: String, m: String): Fragment =
    Fragment.const(s"(case when $c.source_url is not null and $m.role in ('edit', 'owner') then 'view' else ") ++
      role(c, m) ++ Fragment.const(" end)")
}
