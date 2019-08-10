package qwde.plotly;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.plotly.Plot;
import tech.tablesaw.plotly.components.Axis;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Layout;
import tech.tablesaw.plotly.components.Page;
import tech.tablesaw.plotly.traces.ScatterTrace;
import weka.gui.explorer.VisualizePanel.ScatterDefaults;

import java.util.stream.IntStream;

import qwde.ml.MovingAverage;

public class LinePlotRenderer {

    private static Logger logger = LoggerFactory.getLogger(LinePlotRenderer.class);

    public static String renderFrom1d(Double[] data) {

        double[] xData = IntStream.range(0, data.length).asDoubleStream().toArray();
        
        DoubleColumn xColumn = DoubleColumn.create("xcol", xData);
        DoubleColumn yColumn = DoubleColumn.create("ycol", data);

        int window = 10;
        Double[] sma = MovingAverage.simpleMovingAverage(data, window);
        DoubleColumn yColumn2 = DoubleColumn.create("sma", sma);

        Table table = Table.create("table").addColumns(xColumn, yColumn);
        Table table2 = Table.create("table 2").addColumns(xColumn, yColumn2);

        ScatterTrace trace = ScatterTrace.builder(table.nCol("xcol"), table.nCol("ycol"))
                .mode(ScatterTrace.Mode.LINE)
                .build();
        
        ScatterTrace trace2 = ScatterTrace.builder(table2.nCol("xcol"), table2.nCol("sma")).mode(ScatterTrace.Mode.LINE).build();
                        
        Axis xAxis = Axis.builder()
                .title("Time")
                .autoRange(Axis.AutoRange.TRUE)
                .build();

        Axis yAxis = Axis.builder()
                .title("Price")
                .build();

        Layout layout = Layout.builder()
                .title("Stock Prices")
                .xAxis(xAxis)
                .yAxis(yAxis)
                .build();

        Figure figure = new Figure(layout, trace, trace2);

        Page page = Page.pageBuilder(figure, "testdiv").build();
        return page.asJavascript();
    }
}
