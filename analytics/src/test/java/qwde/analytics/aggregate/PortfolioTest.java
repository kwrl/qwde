package qwde.analytics.aggregate;

import com.flextrade.jfixture.JFixture;
import com.google.common.collect.ImmutableList;
import com.google.common.truth.Truth;
import org.junit.jupiter.api.Test;
import qwde.dataprovider.models.CompanyStockData;

import java.util.HashSet;
import java.util.stream.Collectors;

class PortfolioTest {

  @Test
  void portfolio_sameDataThrice_makessense() {
    JFixture fixture = new JFixture();
    CompanyStockData companyStockData = fixture.create(CompanyStockData.class);
    CompanyStockData clone = new CompanyStockData("clone", companyStockData.closePrices, companyStockData.highPrices, companyStockData.lowPrices, companyStockData.volume, companyStockData.timestamps);
    CompanyStockData duplicate = new CompanyStockData("duplicate", companyStockData.closePrices, companyStockData.highPrices, companyStockData.lowPrices, companyStockData.volume, companyStockData.timestamps);

    Portfolio portfolio = new Portfolio(ImmutableList.of(companyStockData, clone, duplicate));

    // All stddev should be the same
    Truth.assertThat(new HashSet<>(portfolio.stockOverviews.stream().map(x -> x.standardDeviation).collect(Collectors.toList()))).hasSize(1);
    // All covariance pairs should be zero. Samples taken here
    Truth.assertThat(portfolio.covariances.get(companyStockData.companyName).get(clone.companyName)).isEqualTo(0.0);
    Truth.assertThat(portfolio.covariances.get(clone.companyName).get(duplicate.companyName)).isEqualTo(0.0);
  }
}
