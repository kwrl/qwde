package qwde.web.plotly;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;

public final class PageRenderer {
  private static Logger logger = LoggerFactory.getLogger(PageRenderer.class);
  private static Configuration cfg = new Configuration(Configuration.VERSION_2_3_29);

  private PageRenderer() {
  }

  static {
    cfg.setClassForTemplateLoading(PageRenderer.class, "/");
    cfg.setDefaultEncoding("UTF-8");
    cfg.setLocale(Locale.US);
  }

  public static String renderFigure(String pageTitle, List<FigureTemplate> figures) {
    @SuppressWarnings("serial")
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
