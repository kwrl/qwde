package qwde.web.plotly;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.IntStream;

import com.google.common.collect.FluentIterable;

import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.plotly.components.Axis;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Layout;
import tech.tablesaw.plotly.traces.AbstractTrace;
import tech.tablesaw.plotly.traces.ScatterTrace;

public final class LinePlotRenderer {

  private LinePlotRenderer() {
  }

  public static <T extends AbstractTrace> Figure scatterPlot(Collection<T> traces, Class<T> traceClass, String graphText, String xAxisText, String yAxisText) {
    Axis xAxis = Axis.builder()
        .title(xAxisText)
        .autoRange(Axis.AutoRange.TRUE)
        .build();

    Axis yAxis = Axis.builder()
        .title(yAxisText)
        .autoRange(Axis.AutoRange.TRUE)
        .build();

    Layout layout = Layout.builder()
        .title(graphText)
        .xAxis(xAxis)
        .yAxis(yAxis)
        .width(1280)
        .height(720)
        .build();

    return new Figure(layout, FluentIterable.from(traces).toArray(traceClass));
  }

  public static ScatterTrace genScatterPlot(List<Double> data, String name) {
    double[] linearXDomain = IntStream.range(0, data.size()).asDoubleStream().toArray();
    Arrays.setAll(linearXDomain, i -> i + 1);

    DoubleColumn xColumn = DoubleColumn.create("xcol", linearXDomain);
    DoubleColumn yColumn = DoubleColumn.create("ycol", data.stream().toArray(Double[]::new));

    Table table = Table.create("table").addColumns(xColumn, yColumn);
    return ScatterTrace.builder(table.nCol("xcol"), table.nCol("ycol"))
        .mode(ScatterTrace.Mode.LINE)
        .opacity(0.5)
        .name(name)
        .build();
  }
}
