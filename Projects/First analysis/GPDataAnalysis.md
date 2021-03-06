Введение
--------

### Почему приложения в Google Play?

В [Kaggle](https://www.kaggle.com/lava18/google-play-store-apps) --
хранилищем с массивов разнообразных данных, я искал датафреймы для
создания первой аналитической работы. Один из первых датафреймов,
который меня заинтересовал -- выгрузка 10 000 приложений из Google Play.

Сразу появились вопросы, а какие категории наиболее популярны? Влияет ли
размер приложения на количество скачиваний? Как сильно влияет рейтинг на
количество скачиваний? Есть ли корреляция между рейтингом и версией
приложения?

На них вы и увидите ответы в этой статье.

Выводы
------

Начнём с выводов, чтобы те, кто не очень хорошо разбирается в
аналитической терминологии смог сразу получить полезную информацию, не
вникая в статистическую терминологию.

1.  Самая популярная категория в Google Play -- GAME.

![](GPDataAnalysis_files/figure-markdown_strict/unnamed-chunk-2-1.png)

1.  Среди игр самый недооценённый -- казульные игры. На него есть спрос,
    при этом конкуренция в категории небольшая.

![](GPDataAnalysis_files/figure-markdown_strict/unnamed-chunk-3-1.png)

1.  Зависимости между факторами(размер, рейтинг и т.д.) и количеством
    скачиваний меняются от категории к категории. Нельзя применять
    выводы, сформированные на всей выборке, к какой-то конкретной
    категории. Каждую категорию необходимо исследовать отдельно.

2.  В категории игр длина приложения отрицательно сказывается на
    количестве скачиваний. Все лидеры имеют короткие названия.

3.  Рейтинг имеет наибольшее положительное влияние на количество
    скачиваний, как среди категории игр, так и по всей выборке, поэтому
    ему всегда стоит уделять пристальное внимание.

Анализ
------

### Цель и гипотезы

Главная цель статьи -- дать понимание базовых метрик в индустрии
приложений для человека, который никогда с ней не сталкивался, и
облегчить понимание определённых базовых принципов работы в этой
области.

<table>
<caption>Основные метрики по категориям (ТОП-5)</caption>
<thead>
<tr class="header">
<th align="left">Category</th>
<th align="right">Installs</th>
<th align="right">Num.of.apps</th>
<th align="right">Downloads.per.app</th>
<th align="right">Mean.price.US</th>
<th align="right">Length.of.App.Name</th>
<th align="right">Mean.rating</th>
<th align="right">Mean.version</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">GAME</td>
<td align="right">10970300000</td>
<td align="right">647</td>
<td align="right">16955641</td>
<td align="right">0.17</td>
<td align="right">21.84</td>
<td align="right">4.25</td>
<td align="right">2.43</td>
</tr>
<tr class="even">
<td align="left">FAMILY</td>
<td align="right">3855600000</td>
<td align="right">896</td>
<td align="right">4303125</td>
<td align="right">0.21</td>
<td align="right">23.83</td>
<td align="right">4.17</td>
<td align="right">4.72</td>
</tr>
<tr class="odd">
<td align="left">TOOLS</td>
<td align="right">2867450000</td>
<td align="right">354</td>
<td align="right">8100141</td>
<td align="right">0.03</td>
<td align="right">23.87</td>
<td align="right">4.08</td>
<td align="right">3.48</td>
</tr>
<tr class="even">
<td align="left">COMMUNICATION</td>
<td align="right">1801450000</td>
<td align="right">109</td>
<td align="right">16527064</td>
<td align="right">0.03</td>
<td align="right">24.15</td>
<td align="right">4.14</td>
<td align="right">23.68</td>
</tr>
<tr class="odd">
<td align="left">PHOTOGRAPHY</td>
<td align="right">1501650000</td>
<td align="right">153</td>
<td align="right">9814706</td>
<td align="right">0.09</td>
<td align="right">25.40</td>
<td align="right">4.17</td>
<td align="right">3.22</td>
</tr>
<tr class="even">
<td align="left">NEWS_AND_MAGAZINES</td>
<td align="right">1190600000</td>
<td align="right">83</td>
<td align="right">14344578</td>
<td align="right">0.00</td>
<td align="right">22.55</td>
<td align="right">4.11</td>
<td align="right">5.14</td>
</tr>
</tbody>
</table>

####### *Если кто-то из читателей знает, как красиво форматировать большие числа, оставляя числовой формат, напишите на <sergeymong1@gmail.com>*

На первый взгляд, между категориями есть значимые различия c разбросом
значений от 4 600 000 до 10 970 300 000 скачиваний.

Особенно по количеству скачиваний выделяется категория: GAME, это же
подтверждает гистограмма распределения установок:

![](GPDataAnalysis_files/figure-markdown_strict/unnamed-chunk-5-1.png)

На ней мы видим выброс максимального значения -- категорию GAME от
остальных графиков.

Для статистического подтверждения воспользуемся критерием Стьюдента и
проведём t-test. Показатель значимости (p-value): 0.01. Со значением
p-value &lt; 0.05 мы не можем принять гипотезу(нулевая гипотеза) о
равенстве значений. Поэтому различия между категориями значимые.

Конкуренция в лидирующих категориях приложений довольно большая, там
содержится 647 и 896 приложений соответственно, но категория игр гораздо
лучше себя показывает по показателю Downloads.per.app (скачиваний на 1
приложение): 16 955 641 против 4 315 610 в среднем по другим категориям.

Если детализируем эти категории по жанрам, то увидим интересную картину:

<table>
<caption>Основные метрики по жанрам</caption>
<thead>
<tr class="header">
<th align="left">Category</th>
<th align="left">Genres</th>
<th align="right">Installs</th>
<th align="right">Num.of.apps</th>
<th align="right">Downloads.per.app</th>
<th align="right">Mean.price</th>
<th align="right">Length.of.App.Name</th>
<th align="right">Mean.rating</th>
<th align="right">Mean.version</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">GAME</td>
<td align="left">Arcade</td>
<td align="right">3272850000</td>
<td align="right">119</td>
<td align="right">27502941</td>
<td align="right">0.09</td>
<td align="right">18.23</td>
<td align="right">4.24</td>
<td align="right">2.01</td>
</tr>
<tr class="even">
<td align="left">GAME</td>
<td align="left">Action</td>
<td align="right">3006950000</td>
<td align="right">232</td>
<td align="right">12960991</td>
<td align="right">0.18</td>
<td align="right">23.25</td>
<td align="right">4.24</td>
<td align="right">2.10</td>
</tr>
<tr class="odd">
<td align="left">TOOLS</td>
<td align="left">Tools</td>
<td align="right">2867450000</td>
<td align="right">354</td>
<td align="right">8100141</td>
<td align="right">0.03</td>
<td align="right">23.87</td>
<td align="right">4.08</td>
<td align="right">3.48</td>
</tr>
<tr class="even">
<td align="left">COMMUNICATION</td>
<td align="left">Communication</td>
<td align="right">1801450000</td>
<td align="right">109</td>
<td align="right">16527064</td>
<td align="right">0.03</td>
<td align="right">24.15</td>
<td align="right">4.14</td>
<td align="right">23.68</td>
</tr>
<tr class="odd">
<td align="left">PHOTOGRAPHY</td>
<td align="left">Photography</td>
<td align="right">1501650000</td>
<td align="right">153</td>
<td align="right">9814706</td>
<td align="right">0.09</td>
<td align="right">25.40</td>
<td align="right">4.17</td>
<td align="right">3.22</td>
</tr>
<tr class="even">
<td align="left">GAME</td>
<td align="left">Casual</td>
<td align="right">1357000000</td>
<td align="right">14</td>
<td align="right">96928571</td>
<td align="right">0.00</td>
<td align="right">18.79</td>
<td align="right">4.46</td>
<td align="right">6.95</td>
</tr>
</tbody>
</table>

Лидерство категории игр увеличивается, а 30% скачиваний приносит 18%
приложений из категории Arcade.

![](GPDataAnalysis_files/figure-markdown_strict/unnamed-chunk-7-1.png)

Самые популярные жанры в категории игр: Arcade, Action, Casual.

![](GPDataAnalysis_files/figure-markdown_strict/unnamed-chunk-8-1.png)

При этом конкуренция по приложениям не соответвует количеству
скачиваний, больше всего приложений в жанре Action.

![](GPDataAnalysis_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Наблюдаем, что с точки зрения конкуренции и спроса самым привлекательным
является жанр Casual. По метрике Downloads.per.app он опережает
ближайшего конкурента на 44 261 905 скачиваний.

### Что влияет на популярность?

Мы выявили самые популярные жанры приложений в категории игры, теперь
давайте попробуем понять, а что влияет на эту популярность? Так ли важен
рейтинг? Длина названия приложения? Размер? Построим регрессионную
линейную модель по выборке из 4261 приложений. Она позволит показать,
как один из факторов влияет на наш целевой показатель -- скачивания.

Потом сравним её показатели с нашей лидирующей категорией -- GAME, и
посмотрим, есть ли какие-то различия. Если есть, то в следующих статьях
более подробно рассмотрим каждую категорию.

Перед тем, как построить регресионную модель, убедимся в том, что мы
имеем дело с нормальным распределением -- это один из критериев
корректной работы модели. Если значение результатов будет больше 0.05,
то мы можем применять линейную регрессию.

    ##                      p.value
    ## Rating          1.769270e-47
    ## Reviews         6.008384e-88
    ## Size            1.158461e-52
    ## Installs        1.345563e-87
    ## Price           2.647167e-91
    ## Current.Ver     7.203320e-91
    ## Length.App.Name 4.095102e-44

Мы видим, что распределение каждой нашей переменной сильно отличается от
нормального. Поэтому мы не сможем применить линейную регрессию.

Но у нас есть возможность проверить корреляцию между переменными. Это
даст меньше информации, однако, в данной ситуации это лучше, чем ничего.
Перед применением теста немного о том, как будем интерпретировать
результаты. Мы считаем, что значимая взаимосвязь между переменными есть,
если значения p.value меньше, чем 0.05.

#### Значимые факторы

Между нижеперечисленными факторами в общей выборке наблюдается
положительная или отрицательная взаимосвязь. То есть, при изменении
одного фактора, наблюдается изменение другого фактора. Если показатель в
первой колонке:

-   от 0 до 1, то при увеличении одного фактора, есть **увеличение**
    другого фактора;
-   от - 1 до 0, то при увеличении одного фактора, есть **уменьшение**
    другого фактора.

Чем ближе значение к 1 или -1, тем сильнее взаимосвязь.

##### Цена и количество скачиваний

    ##       estimate      p.value
    ## cor 0.06635177 1.542886e-05

Есть небольшая положительная корреляция между рейтингом и количеством
установок.

##### Размер и количество скачиваний

    ##      estimate      p.value
    ## cor 0.1172995 1.576463e-14

Наблюдается положительная корреляция. Устоявшаяся мысль о том, что люди
не качают приложения, которые много весят, так как экономят место на
телефоне, теперь не так убедительна.

В следующих статьях попробуем узнать, меняется ли корреляция от
категории. Так как подобное значение может быть связано влиянием
категории игр: более качественные игры больше весят, и при этом в них
играет большее количество людей.

#### Неначимые факторы

Между нижеперечисленными факторами в общей выборке нет никакой
взаимосвязи.

##### Отзывы и количество скачиваний

    ##      estimate p.value
    ## cor 0.6009824       0

##### Цена и количество скачиваний

    ##        estimate   p.value
    ## cor -0.00716579 0.6400526

##### Длина названия и количество скачиваний

    ##        estimate   p.value
    ## cor -0.02970112 0.0525453

##### Версия и количество скачиваний

    ##      estimate p.value
    ## cor -0.004348 0.77661

##### Версия и рейтинг

    ##       estimate    p.value
    ## cor 0.02704704 0.07834766

### Корреляция по категории "игры"

    ##                estimate      p.value
    ## Length.Name -0.19219749 9.107529e-07
    ## Rating       0.19487904 6.492773e-07
    ## Size         0.18691904 1.817787e-06
    ## Price       -0.09324388 1.803103e-02
    ## Current.Ver  0.07776597 4.871300e-02

В категории GAME мы видим, что каждый из факторов имеет значимую
кореляцию с количеством скачиваний.

Из заинтересовавших корреляций: длина названия приложения и количество
скачиваний. Изначальная гипотеза о том, что длинное название хорошо
влияет на поисковую выдачу и увеличивает количество пользователей на
данной выборке не подтверждается, но это не говорит о том, что подобная
взаимосвязь присутствует везде.

Более подробно мы рассмотрим корреляции по категориям в следующих
статьях.

Чтобы лучше исследовать данный набор данных, необходим более продвинутый
статистический и математический аппрат, так как между данными есть
нелинейная взаимосвязь.

Предобработка данных
--------------------

Здесь мы описываем то, каким образом преобразовывался изначальный массив
данных и почему.

Исходные данные представляют собой csv файл с количеством строк в10841
наблюдений, у них следующая структура:

    ## 'data.frame':    10841 obs. of  13 variables:
    ##  $ App           : chr  "Photo Editor & Candy Camera & Grid & ScrapBook" "Coloring book moana" "U Launcher Lite – FREE Live Cool Themes, Hide Apps" "Sketch - Draw & Paint" ...
    ##  $ Category      : Factor w/ 34 levels "1.9","ART_AND_DESIGN",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Rating        : num  4.1 3.9 4.7 4.5 4.3 4.4 3.8 4.1 4.4 4.7 ...
    ##  $ Reviews       : num  159 967 87510 215644 967 ...
    ##  $ Size          : num  19456 14336 8909 25600 2867 ...
    ##  $ Installs      : num  1e+04 5e+05 5e+06 5e+07 1e+05 5e+04 5e+04 1e+06 1e+06 1e+04 ...
    ##  $ Type          : Factor w/ 2 levels "Free","Paid": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Price         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Content.Rating: Factor w/ 7 levels "","Adults only 18+",..: 3 3 3 6 3 3 3 3 3 3 ...
    ##  $ Genres        : chr  "Art & Design" "Art & Design;Pretend Play" "Art & Design" "Art & Design" ...
    ##  $ Last.Updated  : chr  "January 7, 2018" "January 15, 2018" "August 1, 2018" "June 8, 2018" ...
    ##  $ Current.Ver   : num  1 2 1.2 NA 1.1 1 1.1 6.1 2.9 2.8 ...
    ##  $ Android.Ver   : chr  "4.0.3 and up" "4.0.3 and up" "4.0.3 and up" "4.2 and up" ...

Не у всех данных верно представлен формат, в каких-то столбцах вместо
численных переменных мы имеем комментарий, поэтому произведём
преобразование форматов данных:

-   У переменной размера строки с буквенными значениями переведём в NA,
    а весь размер приведём к килобайтам;
-   Количество установок переведём в формат numeric для возможности
    построения графиков и формирования выборок;
-   Так как тип приложения 0 и Free одинаков, все данные типы приведём к
    Free, и сделаем переменную фактором;
-   Цену приведём к числовому формату;
-   Версию приведём к числовому формату.

После преобразования структура имеет следующий вид:

    ## 'data.frame':    10841 obs. of  13 variables:
    ##  $ App           : chr  "Photo Editor & Candy Camera & Grid & ScrapBook" "Coloring book moana" "U Launcher Lite – FREE Live Cool Themes, Hide Apps" "Sketch - Draw & Paint" ...
    ##  $ Category      : Factor w/ 34 levels "1.9","ART_AND_DESIGN",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Rating        : num  4.1 3.9 4.7 4.5 4.3 4.4 3.8 4.1 4.4 4.7 ...
    ##  $ Reviews       : num  159 967 87510 215644 967 ...
    ##  $ Size          : num  19456 14336 8909 25600 2867 ...
    ##  $ Installs      : num  1e+04 5e+05 5e+06 5e+07 1e+05 5e+04 5e+04 1e+06 1e+06 1e+04 ...
    ##  $ Type          : Factor w/ 2 levels "Free","Paid": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Price         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Content.Rating: Factor w/ 7 levels "","Adults only 18+",..: 3 3 3 6 3 3 3 3 3 3 ...
    ##  $ Genres        : chr  "Art & Design" "Art & Design;Pretend Play" "Art & Design" "Art & Design" ...
    ##  $ Last.Updated  : chr  "January 7, 2018" "January 15, 2018" "August 1, 2018" "June 8, 2018" ...
    ##  $ Current.Ver   : num  1 2 1.2 NA 1.1 1 1.1 6.1 2.9 2.8 ...
    ##  $ Android.Ver   : chr  "4.0.3 and up" "4.0.3 and up" "4.0.3 and up" "4.2 and up" ...

Эти данные всё ещё содержат пропуски, но они теперь более корректно
представлены в виде значений NA.

Выборка включает в себя приложения с малым количеством скачиваний и
отзывов, поэтому я дополнительно отфильтровал таблицу, так как
непопулярные приложения не нужны в рамках исследования. Использовал
следующие критерии:

-   Количество скачиваний превышеает 10 000 (Installs &gt; 10000);
-   Текущая версия приложения больше нуля (Current.Ver &gt; 0);
-   Размер приложения больше нуля (Size &gt; 0);
-   У приложения есть хотя бы 50 отзывов (Reviews &gt; 50).

Таким образом мы остекли приложения, которые не пользуются спросом у
людей или которые не прошли стадию релиза. После применения критериев
данные представляют собой следующий вид:

    ## Observations: 4,261
    ## Variables: 12
    ## $ App             <chr> "Google News", "Subway Surfers", "Candy Crush ...
    ## $ Category        <fct> NEWS_AND_MAGAZINES, GAME, GAME, COMMUNICATION,...
    ## $ Rating          <dbl> 3.9, 4.5, 4.4, 4.3, 4.3, 4.3, 4.6, 4.3, 4.5, 4...
    ## $ Reviews         <dbl> 878065, 27725352, 22430188, 4785988, 10486018,...
    ## $ Size            <dbl> 13312.0, 77824.0, 75776.0, 11264.0, 24576.0, 7...
    ## $ Installs        <dbl> 1e+09, 1e+09, 5e+08, 5e+08, 5e+08, 5e+08, 5e+0...
    ## $ Type            <fct> Free, Free, Free, Free, Free, Free, Free, Free...
    ## $ Price           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ Content.Rating  <fct> Teen, Everyone 10+, Everyone, Everyone, Everyo...
    ## $ Genres          <chr> "News & Magazines", "Arcade", "Casual", "Commu...
    ## $ Current.Ver     <dbl> 5.20, 1.90, 1.12, 9.80, 1.40, 5.17, 4.50, 1.49...
    ## $ Length.App.Name <int> 11, 14, 16, 29, 3, 14, 26, 12, 43, 10, 11, 7, ...

На этом преобразования изначального массива данных заверешены. Далее в
статье выборки по категориям мы производили из преобразованного массива
данных.
