package qwde.web.plotly;

import org.apache.commons.lang3.RandomStringUtils;

import tech.tablesaw.plotly.components.Figure;

@SuppressWarnings("unused")
public class FigureTemplate {
  private final Figure figure;
  private final String title;
  private final String text;
  private final String id;

  public FigureTemplate(Figure figure, String title, String text) {
    this.figure = figure;
    this.id = "figure" + RandomStringUtils.randomAlphabetic(10);
    this.title = title;
    this.text = text;
  }

  public String getFigure() {
    return figure.asJavascript(this.id);
  }

  public String getTitle() {
    return title;
  }

  public String getText() {
    return text;
  }

  public String getId() {
    return id;
  }
}
