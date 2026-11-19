package skuaka.core

import java.time.*
import scala.util.Try

object Time {
  def instant(
    s: String
  ): Option[Instant] =
    Try(Instant.parse(s)).orElse(Try(OffsetDateTime.parse(s).toInstant)).toOption

  def zone(
    s: String
  ): Option[ZoneId] = Try(ZoneId.of(s)).toOption
}
