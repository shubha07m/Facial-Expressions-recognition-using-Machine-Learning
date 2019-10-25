# Facial-Expressions-recognition-using-Machine-Learning

Project Proposal

Title: Facial Expressions recognition using Machine Learning [1]
Group members: Sumitra Dey, Shubhabrata Mukherjee

(a)	Description of the problem
Using Microsoft Kinect, researchers [2] have obtained: (a) an image of each frame, identified by a timestamp; (b) a text file containing one hundred coordinates (x, y, z) of points from eyes, nose, eyebrows etc., each line in the data corresponds to points extracted from one frame.

Our primary goal is to identify when a human is talking and when remains silent by classification of the facial expression. We would also like to identify the nature of dialog from the expression if time and complexity permits.


(b)	Description of the dataset (dimensions, names of variables with their description)

The dataset is organized in 36 files: 18 data point files and 18 target files, one pair for each video which compose the dataset. The name of the file refers to each video: the letter corresponding to the user (A and B), name of grammatical facial expression and a specification (target or data points). Coordinates x and y are given in pixels. Coordinates z are given in millimeters.


(c)	Supervised or Unsupervised?
Supervised

(d)	Regression or classification?
Classification
(e)	Comments:
The automated analysis of facial expressions has been widely used in different research areas, such as biometrics or emotional analysis. This project should be an ideal starting point for applying classification techniques for solving real life problems.

Concerns:
Time and complexity of the work are the primary concerns for it, which can be overcome by sufficient literature survey on the topic and the ML techniques to be used.


Ref: 
[1] http://archive.ics.uci.edu/ml/datasets/Grammatical+Facial+Expressions
[2] FREITAS, F. A.; Peres, S. M.; Lima, C. A. M.; BARBOSA, F. V. Grammatical Facial Expressions Recognition with Machine Learning
