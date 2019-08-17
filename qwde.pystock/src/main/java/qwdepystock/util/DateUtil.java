package qwdepystock.util; 

import java.util.Date;
import java.time.LocalDateTime;
import java.time.LocalDate;
import java.time.ZoneId;

public class DateUtil {
  public static Date toDate(LocalDateTime localDateTime) {
    return Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());
  }

  public static Date fromLocalToDate(LocalDate localDateTime) {
    return Date.from(localDateTime.atStartOfDay(ZoneId.systemDefault()).toInstant());
  }

  public static boolean compareDdMmYyyy(LocalDateTime left, LocalDateTime right) {
    return left.getDayOfYear() == right.getDayOfYear() && left.getMonth() == right.getMonth() && left.getYear() == right.getYear();
  }
}

