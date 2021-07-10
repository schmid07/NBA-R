<p align = "center">
<img src = "http://g.recordit.co/aZGcdFsNET.gif" width = "700">
</p>

The table below above adapted from my Week 41/2020 [TidyTuesday](https://github.com/rfordatascience/tidytuesday) [post](https://github.com/schmid07/TidyTuesday_Weekly_Data_Viz_Challenge/tree/main/plots/2020_41) that I also submitted as part of the [RStudio Table Contest](https://blog.rstudio.com/2020/12/23/winners-of-the-2020-rstudio-table-contest/). I had been wanting to explore **Greg Lin's** `reactable` package and figured using my original post, which used the `gt` package, would be a good starting point.

For anyone just getting started with the `reactable` package, I'd recommend checking out the numerous examples and demos Greg Lin put together [here](https://glin.github.io/reactable/index.html) and also **Kyle Cuilla's** `reactablefmtr` [package](https://kcuilla.github.io/reactablefmtr/index.html), which helps to simplify implementing various components of `reactable`. Unfortunately, I was about mid-way through putting this table together before coming across `reactablefmtr` (otherwise I would have used it more, especially for adding in the images), but I did use it for the conditional formatting. The `reactablefmtr` package has a ton of other useful features that I'd like to explore in the future.

Some other resources that I found particularly helpful in getting started with `reactable` were **Tom Mock's** [blog](https://themockup.blog/) posts [here](https://themockup.blog/posts/2020-05-13-reactable-tables-the-rest-of-the-owl/) and [here](https://themockup.blog/posts/2020-05-29-client-side-interactivity-do-more-with-crosstalk/) and the following TidyTuesday, R Studio Table Contest, and Twitter posts from: 

* **Amit Levinson** [(Table)](https://amitlevinson.github.io/TidyTuesday/2021/week3_tate/tate_art.html) [(Code)](https://github.com/AmitLevinson/TidyTuesday/blob/master/2021/week3_tate/tate_art.Rmd) 

* **Georgios Karamanis** [(Table)](https://github.com/gkaramanis/table-contest) [(Code)](https://github.com/gkaramanis/table-contest/blob/main/table-contest.Rmd)  

* **Sharon Machlis** [(Article)](https://www.infoworld.com/article/3543297/how-to-create-tables-in-r-with-expandable-rows.html)

For anyone interested in learning more about the `crosstalk` aspects of the table which allows for the interactivity between the slider and the search box and the table, again Greg Lin has several helpful examples in his demos (linked above). **Emily Riederer** also has a really helpful [tutorial](https://emilyriederer.netlify.app/post/crosstalk/) for getting started with `crosstalk`. A couple other helpful `crosstalk` examples that I came across on Twitter:

* **Sue Wallace** [(Table)](https://sue-wallace.github.io/fatal-force-with-crosstalk/)
[(Code)](https://github.com/sue-wallace/fatal-force-with-crosstalk/blob/master/01.%20crosstalk.Rmd)   

* **Long Nguyen** [(Table and Code)](https://rpubs.com/long39ng/702061)

Last, big thanks to **Amit Levinson** for helping me troubleshoot how to get an interactive post online. He has a helpful blog post on the topic [here](https://amitlevinson.com/blog/sharing-interactive-charts/).

### Original TidyTuesday post from Week 41/2020 (using `gt` package) 

[Code](https://github.com/schmid07/TidyTuesday_Weekly_Data_Viz_Challenge/blob/main/Code/2020_41_bball.R)

![plots/2020_41/2020_41.png](https://raw.githubusercontent.com/schmid07/TidyTuesday_Weekly_Data_Viz_Challenge/main/plots/2020_41/2020_41.png)