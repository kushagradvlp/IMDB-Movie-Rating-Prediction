# IMDB-Movie-Rating-Prediction

IMDb is an online database of information related to films, television programs, home videos, video games, and streaming content online – including cast, production crew and personal biographies, plot summaries, trivia, ratings, and fan and critical reviews. Ratings reflect the quality of every aspect of the film and are becoming an increasingly important factor in the overall success of the film. Ideally, the higher the rating, the better the film. IMDb allows registered users to rate any movie on a scale of 1 to 10. Since the IMDb rating of a movie is important information for the viewer, it is important to make an accurate rating to represent the full range of the movie. Various factors contribute to the high and low IMDb ratings of movies. Some of these factors can be quantified or categorized to build a model for predicting IMDb ratings for movies. The purpose of this project is to build such a statistical model to predict the IMDb ratings of the upcoming 40 movies.

The dataset used has information on 2953 movies from IMDb. It includes characteristics or variables such as movie names, IMDb score, budget, duration and genre, release time and year, the cast and crew, language, and production country. 

The columns such as “title”, “imdb_id“ and “imdb_url” are removed for the analysis as these variables are distinct for each data point. The variables like “genre_shortfilms” and “genre_realitytv“ are also removed from the data analysis since all values in these columns are 0. Hence these will not have any effect on the model. The columns like “main_lang”, ”main_director_name”, ”main_actor1_name”, ”main_actor2_name”, ”main_actor3_name”, “main_producer_name”, ”editor_name”, ”main_production_company”, ”main_production_country” have different written scripts which are transformed to English for better interpretability and uniformity of the script during analysis.
Finally, 46 columns and 2953 records are picked for the overall analysis of data and for the prediction of modeling.
