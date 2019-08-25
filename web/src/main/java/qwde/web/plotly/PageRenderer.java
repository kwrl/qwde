package qwde.web.plotly;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import freemarker.core.ParseException;
import freemarker.template.Configuration;
import freemarker.template.MalformedTemplateNameException;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateNotFoundException;
import freemarker.template.Version;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Page;

public class PageRenderer {
	private static Logger logger = LoggerFactory.getLogger(PageRenderer.class);
	// public static Page renderFigures(List<Figure> figures, int graphsInWidth, int
	// graphsInHeight) {
	// }
	private static Configuration cfg = new Configuration(Configuration.VERSION_2_3_29);

	static {
		cfg.setClassForTemplateLoading(PageRenderer.class, "/");
		cfg.setDefaultEncoding("UTF-8");
		cfg.setLocale(Locale.US);
	}

	public static String renderFigure(String pageTitle, List<FigureTemplate> figures) {
		HashMap<String, Object> input = new HashMap<String, Object>() {
			{
				put("pageTitle", pageTitle);
				put("figures", figures);
			}
		};

		try {
			Template template = cfg.getTemplate("graphpage.ftl");
			StringWriter stringWriter = new StringWriter();
			template.process(input, stringWriter);
			return stringWriter.toString();
		} catch (TemplateException | IOException exception) {
			logger.error("", exception);
			return "error occured, see logs :(";
		} 
	}
}