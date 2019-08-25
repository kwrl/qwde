package qwde.web.plotly;

import java.util.List;
import java.util.Arrays;
import java.util.Collection;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.FluentIterable;

import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.plotly.components.Axis;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Layout;
import tech.tablesaw.plotly.components.Page;
import tech.tablesaw.plotly.traces.AbstractTrace;
import tech.tablesaw.plotly.traces.ScatterTrace;
import tech.tablesaw.plotly.traces.Trace;

import java.util.stream.IntStream;
import java.util.ArrayList;

import qwde.analytics.ml.MovingAverage;

public class LinePlotRenderer {

    private static Logger logger = LoggerFactory.getLogger(LinePlotRenderer.class);

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
    

    public static String renderFrom1d(List<Double> data) {
        double[] testdataX = IntStream.range(0, data.size()).asDoubleStream().toArray();
        Arrays.setAll(testdataX, i -> i + 1);
        Double[] dataAsArray = new Double[data.size()];
        dataAsArray = data.toArray(dataAsArray);
        
        DoubleColumn xColumn = DoubleColumn.create("xcol", testdataX);
        DoubleColumn yColumn = DoubleColumn.create("ycol", dataAsArray);

        List<ScatterTrace> smaPlots = new ArrayList<>();

        for (int window = 10; window <= 100; window += 10) {
            Double[] sma = MovingAverage.simpleMovingAverage(dataAsArray, window);
            DoubleColumn smaColumn = DoubleColumn.create("sma", sma);

            Table smaTable = Table.create("smaTable").addColumns(xColumn, smaColumn);
            
            ScatterTrace smaTrace = ScatterTrace.builder(smaTable.nCol("xcol"), smaTable.nCol("sma")).mode(ScatterTrace.Mode.LINE)
            .name("Window Length: " + window)
            .build(); 

            smaPlots.add(smaTrace);
        }

        Table table = Table.create("table").addColumns(xColumn, yColumn);

        ScatterTrace dataPlot = ScatterTrace.builder(table.nCol("xcol"), table.nCol("ycol"))
                .mode(ScatterTrace.Mode.LINE)
                .opacity(0.5)
                .name("Price")
                .build();
        
        Axis xAxis = Axis.builder()
                .title("Time")
                .autoRange(Axis.AutoRange.TRUE)
                .build();

        Axis yAxis = Axis.builder()
                .title("Price")
                .autoRange(Axis.AutoRange.TRUE)
                .build();

        Layout layout = Layout.builder()
                .title("Stock Prices")
                .xAxis(xAxis)
                .yAxis(yAxis)
                .width(9001)
                .height(9002)
                .build();

        smaPlots.add(dataPlot);

        Figure figure = new Figure(layout, smaPlots.toArray(new ScatterTrace[smaPlots.size()]));
    
        Page page = Page.pageBuilder(figure, "testdiv").build();

        String js = page.asJavascript().replaceFirst("9001,", "screen.width,").replaceFirst("9002,", "screen.height,");
        return js;
    }
}
