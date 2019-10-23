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
    private static final Logger LOG = LoggerFactory.getLogger(PageRenderer.class);
    private static final Configuration CFG = new Configuration(Configuration.VERSION_2_3_29);

    private PageRenderer() {
    }

    static {
        CFG.setClassForTemplateLoading(PageRenderer.class, "/");
        CFG.setDefaultEncoding("UTF-8");
        CFG.setLocale(Locale.US);
    }

    public static String renderFigure(String pageTitle, List<FigureTemplate> figures) {
        @SuppressWarnings("serial")
        HashMap<String, Object> input = new HashMap<>() {
            {
                put("pageTitle", pageTitle);
                put("figures", figures);
            }
        };

        try {
            Template template = CFG.getTemplate("graphpage.ftl");
            StringWriter stringWriter = new StringWriter();
            template.process(input, stringWriter);
            return stringWriter.toString();
        } catch (TemplateException | IOException exception) {
            LOG.error("", exception);
            return "error occured, see logs :(";
        }
    }
}
