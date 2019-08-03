package qwde.plotly;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Page;
import tech.tablesaw.plotly.traces.ScatterTrace;

import java.util.stream.IntStream;

public class LinePlotRenderer {
    private static Logger logger = LoggerFactory.getLogger(LinePlotRenderer.class);

    public static String renderFrom1d(Double[] data) {
        double[] testdataX = IntStream.range(0, data.length).asDoubleStream().toArray();
        DoubleColumn testcolumnX = DoubleColumn.create("xcol", testdataX);
        DoubleColumn testcolumnY = DoubleColumn.create("ycol", data);

        Table testtable = Table.create("test table").addColumns(testcolumnX, testcolumnY);

        ScatterTrace trace = ScatterTrace.builder(testtable.nCol("xcol"), testtable.nCol("ycol"))
                .mode(ScatterTrace.Mode.LINE)
                .showLegend(true)
                .build();

        Figure figuretest = new Figure(trace);

        Page page = Page.pageBuilder(figuretest, "testdiv").build();
        return page.asJavascript();

        // Writer writer = new OutputStreamWriter(outputStream, StandardCharsets.UTF_8);
        //
        // try {
        //     writer.write(output);
        // } catch (IOException exception) {
        //     logger.error("", exception);
        // } finally {
        //     try {
        //         writer.close();
        //     } catch (IOException exception) {
        //         logger.error("", exception);
        //     }
        // }
    }
}
