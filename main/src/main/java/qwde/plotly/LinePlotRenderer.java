package qwde.plotly;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.servlets.SharkToothServlet;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Layout;
import tech.tablesaw.plotly.components.Page;
import tech.tablesaw.plotly.traces.ScatterTrace;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.stream.IntStream;

public class LinePlotRenderer {
    private static Logger logger = LoggerFactory.getLogger(LinePlotRenderer.class);

    public static void renderFrom1d(Double[] data, OutputStream outputStream) {
        double[] testdataX = IntStream.range(0, data.length).asDoubleStream().toArray();
        DoubleColumn testcolumnX = DoubleColumn.create("xcol", testdataX);
        DoubleColumn testcolumnY = DoubleColumn.create("ycol", testdataX);

        Table testtable = Table.create("test table").addColumns(testcolumnX, testcolumnY);
        NumberColumn x = testtable.nCol("xcol");
        NumberColumn y = testtable.nCol("ycol");

        Layout layout = Layout.builder()
                .title("Monthly Boston Armed Robberies Jan. 1966 - Oct. 1975")
                .build();
        ScatterTrace trace = ScatterTrace.builder(x, y)
                .mode(ScatterTrace.Mode.LINE)
                .showLegend(true)
                .build();

        Figure figuretest = new Figure(trace);

        Page page = Page.pageBuilder(figuretest, "testdiv").build();
        String output = page.asJavascript();

        Writer writer = new OutputStreamWriter(outputStream, StandardCharsets.UTF_8);

        try {
            writer.write(output);
        } catch (IOException exception) {
            logger.error("", exception);
        } finally {
            try {
                writer.close();
            } catch (IOException exception) {
                logger.error("", exception);
            }
        }
    }
}
