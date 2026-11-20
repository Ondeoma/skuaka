package skuaka.web

import skuaka.shared.UserBrief
import sbirds.web.Person

// sbirds-lib の画面の部品（.st からは skuaka.web.* として見える）
export sbirds.web.{ApiError, AppNames, Backdrop, Dialog, DialogReq, Dom, Fallback, Images, Observe, Person, Pwa, Toasts}

/** アイコン（parts/Avatar）に出す人. */
extension (u: UserBrief) def person: Person = Person(u.id.toString, u.name, u.avatarUrl)

/** parts/UserPicker で探す人（選んだ Person から UserBrief を引く. 候補にはメールアドレスを添える）. */
object UserSearch extends sbirds.web.PeopleSearch[UserBrief](Api.users, _.person, _.email.getOrElse(""))
