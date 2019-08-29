package qwde.analytics.aggregate;

import java.util.List;

import tech.tablesaw.api.DoubleColumn;

public class StandardDeviation {
  public static List<Double> standardDeviaton(List<Double> data, int windowSize) {
    DoubleColumn xColumn = DoubleColumn.create("data", data.stream().toArray(Double[]::new));
    xColumn.rolling(windowSize).calc(tech.tablesaw.aggregate.AggregateFunctions.standardDeviation);

    return xColumn.asList();
  }
}
