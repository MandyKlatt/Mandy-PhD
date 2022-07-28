* Um den Datensatz in SPSS zu laden, führen Sie diese Syntax bitte vollständig aus.
* - entweder (a) wählen Sie im Menü oben: "Ausführen", "Alles"
* - oder (b) markieren Sie die gesamte Syntax (Strg+A) und klicken Sie auf den Pfeil (Aktuellen Befehl ausführen)

DATA LIST FREE(TAB)
 /CASE (F8.0)
  SERIAL (A4)
  REF (A4)
  QUESTNNR (A4)
  MODE (A16)
  STARTED (DATETIME)
  UI06_05 (F8.0)
  US01_01 (F3.0)
  US01_02 (F3.0)
  US01_03 (F3.0)
  US01_04 (F3.0)
  US01_05 (F3.0)
  US01_06 (F3.0)
  US02_01 (F3.0)
  US02_02 (F3.0)
  US02_03 (F3.0)
  US02_04 (F3.0)
  US02_05 (F3.0)
  US02_06 (F3.0)
  US03_01 (F3.0)
  US03_02 (F3.0)
  US03_03 (F3.0)
  US03_04 (F3.0)
  US03_05 (F3.0)
  US03_06 (F3.0)
  US04_01 (F3.0)
  US04_02 (F3.0)
  US04_03 (F3.0)
  US04_04 (F3.0)
  US04_05 (F3.0)
  US04_06 (F3.0)
  US05_01 (F3.0)
  US05_02 (F3.0)
  US05_03 (F3.0)
  US05_04 (F3.0)
  US05_05 (F3.0)
  US05_06 (F3.0)
  US06_01 (F3.0)
  US06_02 (F3.0)
  US06_03 (F3.0)
  US06_04 (F3.0)
  US06_05 (F3.0)
  US06_06 (F3.0)
  US07_01 (F3.0)
  US07_02 (F3.0)
  US07_03 (F3.0)
  US07_04 (F3.0)
  US07_05 (F3.0)
  US07_06 (F3.0)
  US07_07 (F3.0)
  US08_01 (F3.0)
  US08_02 (F3.0)
  US08_03 (F3.0)
  US08_04 (F3.0)
  US08_05 (F3.0)
  US09_01 (F3.0)
  US09_02 (F3.0)
  US09_03 (F3.0)
  US09_04 (F3.0)
  US09_05 (F3.0)
  US10_01 (F3.0)
  US10_02 (F3.0)
  US10_03 (F3.0)
  US10_04 (F3.0)
  US10_05 (F3.0)
  US10_06 (F3.0)
  US11_01 (F3.0)
  US11_02 (F3.0)
  US11_03 (F3.0)
  US11_04 (F3.0)
  US11_05 (F3.0)
  US12_01 (F3.0)
  US12_02 (F3.0)
  US12_03 (F3.0)
  US12_04 (F3.0)
  US12_05 (F3.0)
  TIME001 (F8.0)
  TIME002 (F8.0)
  TIME003 (F8.0)
  TIME004 (F8.0)
  TIME005 (F8.0)
  TIME006 (F8.0)
  TIME007 (F8.0)
  TIME008 (F8.0)
  TIME009 (F8.0)
  TIME010 (F8.0)
  TIME011 (F8.0)
  TIME012 (F8.0)
  TIME013 (F8.0)
  TIME_SUM (F8.0)
  MAILSENT (DATETIME)
  LASTDATA (DATETIME)
  FINISHED (F1.0)
  Q_VIEWER (F1.0)
  LASTPAGE (F8.0)
  MAXPAGE (F8.0)
  MISSING (F8.0)
  MISSREL (F8.0)
  TIME_RSI (F12.4)
  DEG_TIME (F8.0).
BEGIN DATA
64			base	interview	21-07-2021 17:14:40	101	5	1	3	2	2	3	6	4	1	5	3	2	6	2	5	1	2	3	3	1	1	3	2	3	3	2	4	6	1	2	5	3	2	3	1	2	2	4	3	3	4	1	2	1	4	2	1	2	6	5	6	6	2	5	3	4	6	2	1	5	5	4	1	2	2	2	4	3	1	10	75	70	64	69	71	47	68	50	51	55	37	48	715		
21-07-2021 17:26:35	1	0	13	13	0	0	1.15	13	
65			base	interview	27-07-2021 17:41:18	102	4	5	3	3	1	1	5	6	3	4	2	2	3	4	3	2	1	5	1	2	1	2	5	4	6	3	4	2	1	5	5	3	1	4	2	1	3	5	5	6	5	1	4	2	6	3	2	1	6	2	6	4	3	5	3	3	4	1	1	4	5	4	1	2	3	4	1	6	2	24	81	62	53	77	52	44	63	32	57	54	49	43	691		
27-07-2021 17:52:49	1	0	13	13	0	0	1.1	6	
66			base	interview	28-07-2021 15:40:54	103	5	5	3	3	2	5	5	4	2	6	2	2	2	3	4	3	3	5	2	5	6	2	3	6	4	6	2	5	1	2	6	2	1	5	1	2	6	5	5	3	6	1	1	1	3	3	3	3	6	3	6	6	1	6	4	2	5	2	1	5	6	5	1	3	3	5	2	5	3	12	65	62	81	68	65	48	60	40	52	65	73	53	744		
28-07-2021 15:53:18	1	0	13	13	0	0	1.11	11	
67			base	interview	05-08-2021 15:58:42	104	5	1	2	2	2	2	3	2	1	3	3	1	3	2	2	2	2	4	1	1	1	3	3	5	3	3	2	2	2	3	4	2	1	3	1	2	2	1	1	1	5	1	1	1	5	2	2	1	3	1	5	5	3	2	2	3	5	1	1	3	4	5	2	2	2	2	1	2	3	8	140	41	37	40	57	39	34	27	30	33	30	27	543		
05-08-2021 16:07:45	1	0	13	13	0	0	1.6	39	
69			base	interview	31-08-2021 14:25:11	201	5	1	3	3	5	2	4	3	2	5	4	3	6	4	3	1	3	5	1	2	5	5	5	2	3	2	2	4	2	6	6	2	1	4	1	3	5	3	2	3	5	1	1	1	5	3	3	2	5	4	5	5	2	4	1	5	3	1	4	4	5	2	1	5	5	2	1	5	2	6	114	87	77	115	118	59	75	50	127	58	49	46	
850		31-08-2021 14:41:32	1	0	13	13	0	0	0.94	21	
70			base	interview	01-09-2021 17:06:31	105	2	1	1	6	4	1	2	5	1	6	1	6	2	2	1	1	1	4	1	1	1	3	4	6	5	4	1	1	1	6	6	1	1	6	1	1	1	1	1	2	6	3	2	1	2	2	2	2	5	2	2	2	2	2	2	1	5	1	3	4	2	2	1	1	2	5	1	4	3	15	123	75	97	50	68	30	73	42	68	56	52	58	807	
	01-09-2021 17:19:58	1	0	13	13	0	0	1.08	11	
71			base	interview	15-09-2021 12:02:49	106	5	3	4	4	1	3	5	4	3	2	2	4	2	2	5	2	3	6	3	2	1	5	5	2	3	4	2	3	1	5	3	2	3	1	2	3	6	2	6	2	5	2	1	2	5	3	4	2	4	3	4	5	2	4	4	5	5	2	1	4	3	4	3	4	3	4	3	5	5	8	55	46	52	43	58	30	33	20	27	34	36	37	479		
15-09-2021 12:10:48	1	0	13	13	0	0	1.64	39	
72			base	interview	27-09-2021 18:35:47	202	5	4	1	6	3	1	1	5	2	2	5	1	4	1	2	1	1	5	1	3	5	6	4	1	5	2	3	2	4	5	4	2	1	4	1	1	5	2	5	2	6	1	1	1	2	1	1	1	4	2	4	4	1	5	3	5	6	1	1	4	3	1	1	5	2	3	1	1	1	16	96	72	81	62	83	115	69	39	52	52	67	57	801	
	27-09-2021 18:50:08	1	0	13	13	0	0	0.97	6	
73			base	interview	05-10-2021 16:35:26	203	5	1	2	6	2	3	3	6	1	4	2	1	4	1	1	3	2	6	1	4	3	6	2	3	5	2	1	2	1	6	5	2	1	2	1	1	5	3	2	3	6	1	1	2	2	1	4	3	6	5	5	6	2	4	5	1	5	1	1	6	1	2	1	2	2	3	1	3	1	28	108	112	82	96	68	48	64	68	60	57	74	42	
864		05-10-2021 16:50:33	1	0	13	13	0	0	0.86	2	
75			base	interview	06-10-2021 12:56:44	107	6	2	6	3	1	1	5	6	1	6	2	5	6	2	6	1	1	6	1	4	6	6	2	6	6	6	2	6	1	6	6	4	1	6	1	1	6	6	6	1	6	1	1	1	1	1	6	6	3	1	6	6	3	6	3	4	6	1	1	5	6	2	6	1	4	6	1	2	1	33	46	57	39	55	37	52	53	41	37	58	56	42	606		
06-10-2021 13:06:50	1	0	13	13	0	0	1.23	13	
77			base	interview	13-10-2021 12:48:47	108	4	2	2	5	2	5	4	3	2	4	2	3	3	3	3	2	2	5	2	3	3	5	2	4	5	6	4	2	4	3	4	1	1	3	4	1	3	2	3	2	4	2	2	2	3	2	4	4	3	4	3	3	2	3	4	4	4	2	2	3	3	4	2	3	3	4	2	3	2	21	83	72	44	53	68	46	44	38	45	40	45	31	630		
13-10-2021 12:59:17	1	0	13	13	0	0	1.22	12	
79			base	interview	18-10-2021 16:55:45	109	2	2	3	6	1	5	4	3	2	3	1	2	3	2	2	2	2	5	2	4	1	5	3	5	6	6	2	3	1	2	6	2	1	5	1	1	3	2	6	3	5	1	2	3	3	2	1	1	4	2	4	6	2	3	4	2	5	1	1	2	3	5	2	3	4	3	2	2	3	46	96	62	54	95	119	58	83	40	46	60	69	68	842	
	18-10-2021 17:10:42	1	0	13	13	0	0	0.85	1	
80			base	interview	08-11-2021 14:59:20	110	5	2	3	3	2	2	4	3	1	6	5	3	3	2	6	2	1	3	2	1	2	3	3	5	6	6	2	4	2	4	6	2	3	3	2	2	3	3	4	3	6	2	2	2	3	2	2	2	6	3	1	5	2	5	4	3	5	1	1	3	4	2	4	4	3	3	1	3	2	8	145	75	119	80	74	69	59	46	50	51	48	42	866	
	08-11-2021 15:13:46	1	0	13	13	0	0	1.03	15	
82			base	interview	12-11-2021 12:39:51	204	2	6	3	5	6	6	2	5	1	5	4	5	2	2	3	5	4	6	1	3	3	5	6	4	6	6	4	6	5	5	5	2	2	5	1	3	2	5	3	4	6	2	2	1	1	3	3	4	3	3	6	5	2	3	6	1	5	3	3	3	4	2	2	5	2	5	1	3	3	40	54	49	40	45	39	38	46	29	30	32	33	26	501		
12-11-2021 12:48:13	1	0	13	13	0	0	1.47	23	
85			base	interview	15-11-2021 17:22:53	111	6	1	1	3	2	4	5	4	1	1	2	3	5	4	2	5	2	5	1	1	2	5	5	6	6	6	4	4	4	2	6	1	1	4	1	2	6	6	5	5	5	1	1	1	4	2	3	2	5	2	2	4	1	2	3	4	2	1	3	5	5	3	3	4	4	5	2	3	2	54	78	63	63	75	58	62	45	31	49	41	72	39	730		
15-11-2021 17:35:03	1	0	13	13	0	0	1.03	5	
86			base	interview	26-11-2021 09:49:38	205	6	6	6	6	6	6	5	4	3	6	3	6	6	2	2	4	4	6	2	6	3	2	4	6	6	5	3	4	4	5	2	1	1	2	1	1	2	1	5	3	5	3	1	1	3	1	3	2	6	3	4	5	6	5	4	4	5	1	6	5	2	2	2	2	4	4	2	2	2	7087	112	111	116	176	157	108	100	39	54	52	
88	53	910		26-11-2021 12:07:11	1	0	13	13	0	0	0.62	0	
87			base	interview	17-12-2021 11:46:57	112	4	3	4	5	2	2	5	5	3	2	2	3	5	4	1	3	2	6	3	3	2	3	3	2	6	6	5	4	2	4	6	3	2	3	1	2	5	4	3	3	3	3	1	4	5	2	4	1	4	2	5	3	1	3	2	3	5	1	1	6	5	2	1	4	5	3	2	3	2	38	74	44	40	73	63	51	70	34	49	35	44	40	655		
17-12-2021 11:57:52	1	0	13	13	0	0	1.15	8	
88			base	interview	07-01-2022 11:42:09	206	5	1	2	4	2	3	4	4	1	3	3	1	1	2	4	1	2	4	1	3	3	1	6	5	4	5	3	5	2	5	3	1	1	4	1	2	3	2	5	2	4	2	1	2	2	2	2	1	3	1	4	4	1	3	4	4	3	1	3	2	3	2	2	3	2	3	2	5	1	15	76	59	54	48	66	32	67	28	31	38	55	35	604		
07-01-2022 11:52:13	1	0	13	13	0	0	1.34	17	
89			base	interview	10-01-2022 14:26:45	113																																																																						21													21		10-01-2022 14:27:06	0	0	1	1	0	0	1.86	43	
90			base	interview	10-01-2022 14:27:37	113	4	1	1	4	3	3	4	6	1	6	3	2	2	2	1	3	3	6	1	2	3	4	1	2	2	4	2	2	1	3	5	3	1	3	1	1	3	2	4	1	5	1	1	1	2	1	1	1	2	1	5	4	1	4	2	2	3	1	2	4	3	3	1	5	2	4	1	2	1	11	30	72	49	39	52	55	64	28	32	34	35	40	541		
10-01-2022 14:36:38	1	0	13	13	0	0	1.52	28	
91			base	interview	11-01-2022 17:14:35	114	1	1	1	6	3	6	2	5	1	2	3	2	5	1	2	2	4	6	1	6	6	1	5	5	6	6	1	1	5	2	6	1	1	6	1	1	1	2	5	5	6	1	2	1	1	1	1	3	5	1	2	6	6	3	3	2	6	2	1	1	6	4	1	6	2	5	2	1	2	43	74	63	59	54	57	42	53	31	31	47	43	38	635		
11-01-2022 17:25:11	1	0	13	13	0	0	1.17	9	
92			base	interview	17-01-2022 10:48:04	207	4	2	2	5	4	2	3	4	2	2	2	2	3	2	3	2	2	5	2	3	3	2	4	4	4	3	2	3	3	4	4	2	2	4	1	2	3	2	3	3	3	2	2	4	3	2	3	2	3	2	3	4	2	3	3	3	4	2	2	3	2	2	2	3	2	3	2	3	4	4844	58	61	47	66	59	59	63	33	34	40	45	41	
645		17-01-2022 12:18:54	1	0	13	13	0	0	1.07	7	
93			base	interview	19-01-2022 10:31:06	115	3	1	2	3	1	2	3	4	2	4	2	1	3	4	3	1	1	4	2	1	3	2	3	4	4	2	3	2	1	3	4	2	1	3	1	3	4	1	4	4	4	1	2	3	3	2	2	1	3	1	4	4	3	3	2	4	4	1	2	5	4	2	1	3	3	3	1	3	2	2248	62	65	61	92	73	66	56	61	41	80	50	54	
800		19-01-2022 11:21:15	1	0	13	13	0	0	0.86	3	
94			base	interview	24-01-2022 14:49:20	116	5	2	4	5	1	3	5	4	3	3	2	1	6	3	4	1	2	4	2	4	3	4	2	2	4	6	4	3	1	4	5	3	2	4	1	1	5	4	4	3	4	1	2	2	3	2	4	3	5	2	2	4	2	4	4	4	5	1	3	5	4	3	1	2	4	3	2	3	2	68	76	67	49	72	90	57	72	72	56	50	40	60	795		
24-01-2022 15:03:09	1	0	13	13	0	0	0.91	2	
95			base	interview	31-01-2022 14:40:10	117	4	2	3	5	1	4	3	4	1	5	2	2	3	2	4	2	2	5	2	2	2	5	4	4	4	4	1	2	3	5	5	4	2	5	1	3	3	3	4	2	5	3	2	4	3	2	4	2	4	1	3	5	2	4	3	4	3	1	2	3	2	3	1	3	4	3	1	2	2	62	91	73	68	68	65	87	76	43	49	111	45	48	826	
	31-01-2022 14:54:56	1	0	13	13	0	0	0.86	1	
96			base	interview	14-02-2022 12:35:22	118	3	2	2	5	1	5	2	5	1	6	2	2	2	3	2	5	5	5	1	3	2	6	2	2	6	6	2	3	1	3	6	2	2	5	3	2	2	3	5	2	6	1	1	1	5	1	5	2	3	2	5	6	6	3	3	2	2	1	2	3	2	3	5	1	2	3	1	2	3	58	69	77	60	74	72	73	82	40	46	74	71	55	851		
14-02-2022 12:49:33	1	0	13	13	0	0	0.87	1	
97			base	interview	16-02-2022 12:35:08	119	5	2	3	5	2	2	4	4	1	5	2	1	6	3	5	2	2	6	1	4	5	4	5	6	3	3	3	2	1	6	5	2	1	5	1	3	3	4	6	5	6	2	2	4	3	2	1	1	4	2	3	5	2	5	4	2	6	1	1	6	5	4	1	2	5	4	2	5	3	39	76	65	69	67	109	63	67	29	61	47	62	31	741	
	16-02-2022 12:48:13	1	0	13	13	0	0	0.98	3	
100			base	interview	28-02-2022 15:47:09	120	5	1	3	4	1	1	5	3	1	3	4	2	2	3	4	2	2	6	3	1	1	3	4	6	4	5	1	3	2	4	5	1	1	4	1	1	2	3	3	2	6	1	1	2	4	3	3	1	3	2	5	5	1	4	3	2	5	1	1	3	4	1	1	1	3	3	1	4	2	54	77	78	52	56	61	42	62	30	49	49	58	40	708	
	28-02-2022 15:58:57	1	0	13	13	0	0	1.05	5	
101			base	interview	02-03-2022 11:01:05	121	4	1	3	5	3	4	4	4	1	6	3	1	3	2	3	1	2	6	1	4	3	2	5	6	5	4	3	2	1	4	3	1	2	5	1	1	5	3	3	2	6	1	1	1	3	2	1	1	5	1	6	2	2	4	4	4	6	1	3	1	2	4	3	4	2	2	5	2	1	3058	91	75	101	50	72	60	66	51	68	69	58	48	
848		02-03-2022 12:05:32	1	0	13	13	0	0	0.81	1	
102			base	interview	08-04-2022 13:00:02	208	5	2	2	5	4	2	4	3	3	2	5	4	5	4	3	2	2	4	2	1	2	3	4	1	5	5	4	5	4	5	3	1	1	2	1	1	4	2	4	2	6	2	3	1	1	1	1	1	6	1	6	6	2	6	6	6	6	1	1	6	6	3	1	2	4	4	2	5	4	123	194	100	138	77	71	168	127	38	72	58	80	
47	1003		08-04-2022 13:21:36	1	0	13	13	0	0	0.66	0	
103			base	interview	13-04-2022 12:26:06	209	3	2	1	5	5	4	3	3	1	5	4	2	5	3	2	1	1	6	1	2	3	2	3	3	3	4	4	3	1	4	4	2	2	3	1	3	4	1	3	4	6	1	1	2	2	2	4	2	5	1	6	6	2	3	4	4	6	1	3	6	5	2	1	3	3	3	2	4	3	44	59	54	50	46	67	53	62	46	56	40	43	36	656	
	13-04-2022 12:37:03	1	0	13	13	0	0	1.12	8	
104			base	interview	19-04-2022 16:46:45	122	4	2	3	5	1	5	4	4	1	6	3	2	3	2	2	3	4	6	1	5	4	3	4	5	5	4	2	2	2	5	6	4	2	5	1	3	4	4	5	4	6	1	3	4	4	1	2	1	4	1	3	5	2	5	4	3	6	2	1	6	5	2	1	1	4	3	1	4	2	37	121	122	150	84	118	94	132	60	66	95	79	
87	982		19-04-2022 17:07:31	1	0	13	13	0	0	0.63	0	
105			base	interview	26-04-2022 17:34:41	210	4	1	3	2	1	2	3	4	1	6	3	2	3	2	4	3	2	5	1	2	3	1	4	4	4	5	4	5	3	5	6	2	2	4	2	2	2	3	5	6	6	3	3	2	2	2	2	2	5	2	5	4	2	6	2	6	5	1	3	4	5	5	2	3	2	5	1	5	4	77	102	78	113	91	161	127	110	39	92	94	85	
53	965		26-04-2022 17:55:04	1	0	13	13	0	0	0.64	0	
116			base	interview	05-05-2022 15:48:23	211	3	1	1	6	1	4	2	5	1	6	1	1	5	1	4	1	1	6	1	5	3	1	5	6	1	1	2	2	2	6	1	1	2	5	1	1	3	5	3	2	5	1	2	2	1	1	2	1	3	2	6	5	1	4	3	6	4	1	1	2	6	3	1	5	2	6	6	2	2	77	94	99	105	172	96	58	73	38	50	53	56	47	
914		05-05-2022 16:05:22	1	0	13	13	0	0	0.79	0	
118			base	interview	05-05-2022 16:59:36	212	6	1	3	2	2	2	5	3	2	6	1	2	6	5	6	4	5	3	1	2	2	6	6	6	5	5	3	6	2	5	5	2	2	6	1	3	5	5	6	2	5	2	1	2	5	1	5	2	4	5	6	6	4	6	4	6	6	1	2	6	6	4	2	5	6	4	2	6	3	2693	126	99	66	76	103	268	41	24	26	23	31	
15	724		17-05-2022 17:11:55	1	0	13	13	0	0	1.2	22	
119			base	interview	19-05-2022 14:55:42	213	5	2	3	3	2	2	2	6	1	2	2	4	5	1	3	1	3	6	1	1	1	1	3	3	6	6	3	3	2	3	6	1	1	4	1	3	5	4	4	3	5	1	1	2	2	2	2	2	3	3	3	3	2	4	4	4	5	1	1	5	4	2	2	2	3	4	2	3	3	32	122	96	171	97	119	79	100	50	99	73	75	96	
942		19-05-2022 15:15:51	1	0	13	13	0	0	0.66	1	
121			base	interview	20-05-2022 15:37:13	123	4	5	2	5	2	2	4	2	2	1	1	2	2	2	2	1	5	6	1	2	2	3	1	2	6	5	4	3	2	1	4	1	1	2	1	1	3	2	5	1	2	1	1	2	6	1	4	1	1	2	4	5	1	4	5	5	5	2	2	2	5	2	1	2	3	3	2	2	1	87	57	58	85	73	57	55	62	36	41	51	60	47	769	
	20-05-2022 15:50:02	1	0	13	13	0	0	0.99	4	
126			base	interview	03-06-2022 14:55:43	124	5	1	3	4	2	1	4	3	1	1	3	3	6	5	2	2	1	3	2	1	3	5	4	2	2	6	2	3	1	5	4	1	1	5	2	3	4	3	5	2	5	2	1	2	4	4	2	1	6	2	6	5	6	5	1	1	5	1	3	6	4	3	1	5	5	2	1	5	1	33	99	46	48	53	50	46	76	70	46	41	64	37	709	
	03-06-2022 15:07:32	1	0	13	13	0	0	1.09	8	
130			base	interview	07-06-2022 16:51:27	216	3	2	2	4	1	3	5	2	1	2	2	3	2	2	2	2	2	6	2	3	2	2	2	2	2	4	2	3	2	3	5	2	2	4	1	2	1	1	4	1	5	2	2	1	4	1	1	1	2	1	1	1	1	4	3	3	3	1	2	4	3	2	2	4	3	3	3	3	3	71	111	72	462	105	43	64	72	35	28	40	50	36	
789		07-06-2022 17:11:16	1	0	13	13	0	0	0.96	7	
131			base	interview	07-06-2022 17:17:48	214	5	3	1	6	4	6	6	3	1	6	6	1	4	2	2	1	1	6	1	2	1	1	5	6	2	2	1	3	3	5	6	2	2	4	1	2	6	2	6	4	6	1	4	3	3	2	3	3	6	3	3	6	1	4	3	5	5	1	2	4	4	2	1	2	2	4	1	6	2	10	78	57	23	15	17	18	34	16	43	21	14	35	381	
	07-06-2022 17:24:09	1	0	13	13	0	0	2.22	76	
132			base	interview	07-06-2022 17:27:11	215	3	1	1	3	1	5	3	4	1	1	2	2	5	2	5	4	1	6	1	1	5	6	3	6	6	6	5	3	3	5	4	1	1	4	1	2	4	3	2	3	6	2	1	1	4	2	1	3	4	2	2	3	1	4	5	5	4	1	2	3	4	4	1	2	3	4	1	4	3	12	24	18	18	26	16	49	32	28	29	15	11	49	327	
	07-06-2022 17:32:38	1	0	13	13	0	0	2.35	87	
133			base	interview	08-06-2022 11:34:24	125	3	2	3	2	2	2	4	2	2	4	3	3	4	2	3	2	2	4	1	1	1	2	3	4	2	2	2	3	3	4	4	3	2	3	1	1	3	2	3	2	3	1	2	3	2	2	3	1	3	2	4	3	1	3	2	3	5	2	2	3	2	2	2	3	2	4	2	3	3	48	56	73	49	60	50	55	65	35	35	39	57	35	657	
	08-06-2022 11:45:21	1	0	13	13	0	0	1.13	8	
135			base	interview	13-06-2022 12:51:49	217	4	1	1	6	4	2	1	2	3	5	1	6	2	1	1	2	4	6	4	2	6	1	1	1	6	3	2	1	1	4	6	1	1	3	1	3	3	2	4	1	6	2	1	1	2	2	4	5	2	3	2	2	1	2	2	1	4	1	2	2	1	1	1	2	1	4	1	3	1	39	54	54	35	48	48	39	41	22	28	31	36	29	504	
	13-06-2022 13:00:14	1	0	13	13	0	0	1.48	24	
136			base	interview	16-06-2022 09:39:15	218	6	3	4	6	1	3	4	3	2	3	1	2	2	2	2	3	4	6	1	1	1	3	6	2	6	1	2	3	1	6	4	1	1	3	1	2	2	2	1	2	5	1	2	1	3	2	3	1	4	2	2	5	1	6	3	1	3	1	1	4	5	1	1	3	4	2	1	2	2	50	52	53	71	57	42	52	42	25	38	33	58	29	602	
	16-06-2022 09:49:18	1	0	13	13	0	0	1.26	15	
137			base	interview	16-06-2022 11:23:00	219	4	2	3	3	2	4	5	2	2	5	4	3	3	2	3	3	2	4	2	2	3	4	3	3	3	6	3	4	3	4	3	2	1	3	2	2	3	3	3	2	6	1	2	2	2	2	3	3	5	3	4	4	3	3	2	3	3	2	2	4	5	3	1	4	3	3	2	2	3	26	85	53	46	45	44	44	39	22	82	48	97	37	668	
	16-06-2022 11:34:08	1	0	13	13	0	0	1.23	15	
138			base	interview	07-07-2022 11:43:57	126	3	2	2	4	1	2	2	4	2	4	2	2	3	2	3	2	2	5	2	2	3	3	2	5	4	3	3	2	4	4	5	2	1	4	3	1	3	3	2	2	3	1	1	2	4	1	1	1	3	3	3	4	2	3	2	2	6	1	2	3	2	2	2	3	2	5	2	2	3	48	262	71	96	57	62	65	61	36	48	65	43	44	
776		07-07-2022 11:59:56	1	0	13	13	0	0	0.91	2	
139			base	interview	08-07-2022 11:47:41	127	3	6	6	1	4	1	2	2	6	2	4	6	4	3	6	5	2	1	6	3	3	2	5	6	1	1	3	3	6	2	2	6	5	2	6	6	1	2	2	2	2	6	5	2	5	2	2	2	2	5	5	1	3	4	2	5	2	6	6	1	1	2	6	5	4	2	6	5	2	17	103	100	95	96	63	51	61	49	47	89	42	52	
865		08-07-2022 12:02:06	1	0	13	13	0	0	0.97	7	
END DATA.


VARIABLE WIDTH  SERIAL REF QUESTNNR MODE (8) STARTED MAILSENT LASTDATA (20)
.


**** Variable und Value Labels *********************************************************************************************


VARIABLE LABELS
 CASE 'Interview-Nummer (fortlaufend)'
 SERIAL 'Seriennummer (sofern verwendet)'
 REF 'Referenz (sofern im Link angegeben)'
 QUESTNNR 'Fragebogen, der im Interview verwendet wurde'
 MODE 'Interview-Modus'
 STARTED 'Zeitpunkt zu dem das Interview begonnen hat (Europe/Berlin)'
 UI06_05 'ID: Meine persönliche ID'
 US01_01 'Szenario 1: a) Sie kann die Schülerin zurechtweisen: „Du weißt doch, dass ihr nicht in der Klasse herumlaufen sollt?“'
 US01_02 'Szenario 1: b) Sie kann der Schülerin ein Zeichen geben, welches deutlich macht, dass sie sich hinsetzen soll.'
 US01_03 'Szenario 1: c) Sie kann eine kurze Aufforderung aussprechen: „Setz dich jetzt bitte auf deinen Platz!“'
 US01_04 'Szenario 1: d) Sie kann das Verhalten der Schülerin ignorieren.'
 US01_05 'Szenario 1: e) Sie kann die Schülerin in ihren Vortrag einbeziehen, indem sie ihr eine Frage zum Thema stellt.'
 US01_06 'Szenario 1: f) Sie kann auf die nächste Pause warten, um mit der Schülerin über ihr Verhalten zu sprechen.'
 US02_01 'Szenario 2: a) Sie kann zum störenden Schüler sagen: „Kannst du jetzt mal bitte aufhören, mit deinem Nachbarn zu reden?“'
 US02_02 'Szenario 2: b) Sie kann das Verhalten des störenden Schülers ignorieren, weil er leise spricht.'
 US02_03 'Szenario 2: c) Sie kann dem Schüler einen ernsten Blick zuwerfen und nach vorn zur vortragenden Gruppe deuten.'
 US02_04 'Szenario 2: d) Sie kann die Störung als Gelegenheit ergreifen, um mit der Klasse über die Klassenregeln zu diskutieren.'
 US02_05 'Szenario 2: e) Sie kann den Schüler auffordern, etwas zum Vortrag der Schüler:innengruppe zu sagen.'
 US02_06 'Szenario 2: f) Sie kann den Namen des Schülers nennen und das „Leisezeichen“ zeigen.'
 US03_01 'Szenario 3: a) Sie kann mit lauter Stimme die ganze Klasse ermahnen.'
 US03_02 'Szenario 3: b) Sie kann einzelne auffällige Schüler:innen ermahnen.'
 US03_03 'Szenario 3: c) Sie kann die Gruppenarbeit unterbrechen und alle Schüler:innen an das Einhalten der Regeln erinnern.'
 US03_04 'Szenario 3: d) Während sich die Lehrkraft nacheinander nach dem Aufgabenstand an den einzelnen Gruppentischen erkundigt, kann sie die Schüler:innen ermahnen, die die Regeln nicht einhalten.'
 US03_05 'Szenario 3: e) Sie kann zu den einzelnen Gruppentischen gehen und an die vereinbarten Regeln erinnern.'
 US03_06 'Szenario 3: f) Sie kann das Wechseln der Gruppentische ignorieren.'
 US04_01 'Szenario 4: a) Sie kann mit dem Schüler Blickkontakt aufnehmen und mit einer Geste auf das Etui verweisen.'
 US04_02 'Szenario 4: b) Sie kann abwarten, ob der Schüler mit dem Etui zu spielen beginnt, und erst dann eingreifen, um den Unterrichtsfluss nicht zu stören.'
 US04_03 'Szenario 4: c) Sie kann den Schüler ins Unterrichtsgespräch einbeziehen, indem sie ihm eine Frage stellt.'
 US04_04 'Szenario 4: d) Sie kann zum Schüler gehen und sein Etui in ihre Schultasche legen; dabei redet sie weiter.'
 US04_05 'Szenario 4: e) Sie kann den Schüler bitten zu wiederholen, was sie bezüglich der Etuis gesagt hat.'
 US04_06 'Szenario 4: f) Sie kann mit den Schüler:innen besprechen, warum das Etui vom Tisch geräumt werden sollte.'
 US05_01 'Szenario 5: a) Sie kann den Unterricht fortsetzen, ohne sich irritieren zu lassen.'
 US05_02 'Szenario 5: b) Sie kann mitlachen, nachdem sie sich über das Wohlbefinden des Kindes informiert hat.'
 US05_03 'Szenario 5: c) Sie kann einen kurzen Moment abwarten und weitere aufkommende Albernheiten unterbinden.'
 US05_04 'Szenario 5: d) Sie kann das vom Stuhl gefallene Kind ermahnen und bitten, beim nächsten Mal besser aufzupassen.'
 US05_05 'Szenario 5: e) Sie kann mit einem kurzen Bewegungsspiel fortfahren, das alle Kinder einbezieht, um einen reibungslosen Übergang zur Besprechung der Gruppenarbeit zu schaffen.'
 US05_06 'Szenario 5: f) Sie kann den Stuhlkreis auflösen und die Schüler:innen begeben sich wieder an ihre Plätze, um von dort aus die Ergebnisse der Gruppenarbeit zu besprechen.'
 US06_01 'Szenario 6: a) Sie kann sich darauf verlassen, dass die Schüler:innen die Uhr im Blick haben.'
 US06_02 'Szenario 6: b) Bevor die Gruppenarbeit startet, kann sie einen Zeitrahmen vorgeben.'
 US06_03 'Szenario 6: c) Sie kann einen Zeitrahmen vor Beginn der Gruppenarbeit vorgeben und zusätzlich die Minutenuhr stellen.'
 US06_04 'Szenario 6: d) Sie kann allen Schüler:innen so lange Zeit lassen, wie jede einzelne Gruppe braucht.'
 US06_05 'Szenario 6: e) Sie kann pro Gruppe eine:n „Zeitmanager:in&quot; bestimmen, der/die die Zeit im Blick behält.'
 US06_06 'Szenario 6: f) Sie kann der Klasse in regelmäßigen Abständen mitteilen, wie viel Zeit noch zur Verfügung steht.'
 US07_01 'Szenario 7: a) Sie kann es den fertigen Schüler:innen überlassen, sich selbst zu beschäftigen.'
 US07_02 'Szenario 7: b) Sie kann nacheinander die Ergebnisse der fertigen Schüler:innen kontrollieren und sie beauftragen, die Fehler zu korrigieren.'
 US07_03 'Szenario 7: c) Sie kann den fertigen Schüler:innen erlauben, sich ein Würfelspiel zu nehmen und gemeinsam zu spielen.'
 US07_04 'Szenario 7: d) Sie kann den fertigen Schüler:innen die als nächstes anstehenden Aufgaben zur Bearbeitung geben.'
 US07_05 'Szenario 7: e) Sie kann die Stillarbeit vorzeitig abbrechen und den Schüler:innen ihre noch unbearbeiteten Aufgaben als Hausaufgabe aufgeben.'
 US07_06 'Szenario 7: f) Sie kann den fertigen Schüler:innen eine zum Thema passende Extraaufgabe geben.'
 US07_07 'Szenario 7: g) Sie kann die fertigen Schüler:innen auffordern, den langsameren Schüler:innen zu helfen.'
 US08_01 'Szenario 8: a) Sie kann die Schüler:innen Lose ziehen lassen.'
 US08_02 'Szenario 8: b) Sie kann es den Schüler:innen überlassen, sich zu Gruppen zusammenzufinden.'
 US08_03 'Szenario 8: c) Sie kann die Gruppeneinteilung vornehmen.'
 US08_04 'Szenario 8: d) Sie kann die Gruppen so zusammensetzen, wie die Schüler:innen schon zusammensitzen.'
 US08_05 'Szenario 8: e) Sie kann auf eine Gruppeneinteilung zurückgreifen, die die Schüler:innen schon kennen.'
 US09_01 'Szenario 9: a) Sie kann während eines Unterrichtsgesprächs zu jedem Kind gehen und sich die HA zeigen lassen.'
 US09_02 'Szenario 9: b) Sie kann ein Kind die HA einsammeln lassen und sie während einer Stillarbeitsphase kontrollieren.'
 US09_03 'Szenario 9: c) Sie kann zu Beginn der Stunde zu jedem Kind gehen und die HA kontrollieren.'
 US09_04 'Szenario 9: d) Sie kann jedem Kind nach vorn ans Pult kommen lassen, um die HA zu kontrollieren.'
 US09_05 'Szenario 9: e) Sie kann während einer Stillarbeitsphase zu jedem Kind gehen und sich die HA zeigen lassen.'
 US10_01 'Szenario 10: a) Sie kann das Lied unterbrechen, um die störenden Schüler:innen zurechtzuweisen.'
 US10_02 'Szenario 10: b) Sie kann abwarten, ob die Störung anhält.'
 US10_03 'Szenario 10: c) Sie kann Sanktionen für Störungen des Rituals festlegen.'
 US10_04 'Szenario 10: d) Sie kann die störenden Schüler:innen aus dem Sitzkreis ausschließen.'
 US10_05 'Szenario 10: e) Sie kann das Ritual verändern, z.B. durch liedbegleitende Bewegungen, damit die Schüler:innen aktiv mit einbezogen werden.'
 US10_06 'Szenario 10: f) Sie kann die Schüler:innen abstimmen lassen, ob die Klasse das Begrüßungsritual beibehalten soll.'
 US11_01 'Szenario 11: a) Sie kann sich Gehör verschaffen, indem sie mit ihrer Stimme die Klasse übertönt und die Schüler:innen zur Ruhe ermahnt.'
 US11_02 'Szenario 11: b) Sie kann jede:n störende:n Schüler:in immer prompt auffordern, aufmerksam zu sein.'
 US11_03 'Szenario 11: c) Sie kann das Ende der Stunde abwarten und mit den störenden Schüler:innen Einzelgespräche über Verhaltensregeln führen.'
 US11_04 'Szenario 11: d) Sie kann in der nächsten Unterrichtsstunde mit den Schüler:innen Klassenregeln erarbeiten.'
 US11_05 'Szenario 11: e) Sie kann kleine Belohnungen an die Kinder verteilen, die besonders leise sind und gut mitarbeiten.'
 US12_01 'Szenario 12: a) Sie kann die aufrufenden Schüler:innen zur Eile mahnen.'
 US12_02 'Szenario 12: b) Sie kann jedem aufrufenden Kind die Zeit geben, die es braucht, seine Wahl zu treffen.'
 US12_03 'Szenario 12: c) Sie kann ein Zeitlimit setzen und die Schüler:innen loben, die das Limit einhalten.'
 US12_04 'Szenario 12: d) Sie kann die Schüler:innen ermahnen, dass sie die Meldekette abbricht, wenn die Aufrufenden nicht zügig ihre:n Nachfolger:innen aufrufen.'
 US12_05 'Szenario 12: e) Sie kann die ungeduldigen Schüler:innen um Ruhe und Geduld bitten.'
 TIME001 'Verweildauer Seite 1'
 TIME002 'Verweildauer Seite 2'
 TIME003 'Verweildauer Seite 3'
 TIME004 'Verweildauer Seite 4'
 TIME005 'Verweildauer Seite 5'
 TIME006 'Verweildauer Seite 6'
 TIME007 'Verweildauer Seite 7'
 TIME008 'Verweildauer Seite 8'
 TIME009 'Verweildauer Seite 9'
 TIME010 'Verweildauer Seite 10'
 TIME011 'Verweildauer Seite 11'
 TIME012 'Verweildauer Seite 12'
 TIME013 'Verweildauer Seite 13'
 TIME_SUM 'Verweildauer gesamt (ohne Ausreißer)'
 MAILSENT 'Versandzeitpunkt der Einladungsmail (nur für nicht-anonyme Adressaten)'
 LASTDATA 'Zeitpunkt als der Datensatz das letzte mal geändert wurde'
 FINISHED 'Wurde die Befragung abgeschlossen (letzte Seite erreicht)?'
 Q_VIEWER 'Hat der Teilnehmer den Fragebogen nur angesehen, ohne die Pflichtfragen zu beantworten?'
 LASTPAGE 'Seite, die der Teilnehmer zuletzt bearbeitet hat'
 MAXPAGE 'Letzte Seite, die im Fragebogen bearbeitet wurde'
 MISSING 'Anteil fehlender Antworten in Prozent'
 MISSREL 'Anteil fehlender Antworten (gewichtet nach Relevanz)'
 TIME_RSI 'Maluspunkte für schnelles Ausfüllen'
 DEG_TIME 'Maluspunkte für schnelles Ausfüllen'
.


VALUE LABELS
 /US01_01 US01_02 US01_03 US01_04 US01_05 US01_06 US02_01 US02_02 US02_03
 US02_04 US02_05 US02_06 US03_01 US03_02 US03_03 US03_04 US03_05 US03_06
 US04_01 US04_02 US04_03 US04_04 US04_05 US04_06 US05_01 US05_02 US05_03
 US05_04 US05_05 US05_06 US06_01 US06_02 US06_03 US06_04 US06_05 US06_06
 US07_01 US07_02 US07_03 US07_04 US07_05 US07_06 US07_07 US08_01 US08_02
 US08_03 US08_04 US08_05 US09_01 US09_02 US09_03 US09_04 US09_05 US10_01
 US10_02 US10_03 US10_04 US10_05 US10_06 US11_01 US11_02 US11_03 US11_04
 US11_05 US12_01 US12_02 US12_03 US12_04 US12_05
 1 '[-1-]' 2 '[-2-]' 3 '[-3-]' 4 '[-4-]' 5 '[-5-]' 6 '[-6-]'
 -9 'nicht beantwortet'
 /FINISHED 0 'abgebrochen' 1 'ausgefüllt'
 /Q_VIEWER 0 'Teilnehmer' 1 'Durchklicker'
.

MISSING VALUES
 US01_01 US01_02 US01_03 US01_04 US01_05 US01_06 US02_01 US02_02 US02_03
 US02_04 US02_05 US02_06 US03_01 US03_02 US03_03 US03_04 US03_05 US03_06
 US04_01 US04_02 US04_03 US04_04 US04_05 US04_06 US05_01 US05_02 US05_03
 US05_04 US05_05 US05_06 US06_01 US06_02 US06_03 US06_04 US06_05 US06_06
 US07_01 US07_02 US07_03 US07_04 US07_05 US07_06 US07_07 US08_01 US08_02
 US08_03 US08_04 US08_05 US09_01 US09_02 US09_03 US09_04 US09_05 US10_01
 US10_02 US10_03 US10_04 US10_05 US10_06 US11_01 US11_02 US11_03 US11_04
 US11_05 US12_01 US12_02 US12_03 US12_04 US12_05 (-8,-9)
.

