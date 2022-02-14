(* オブジェクトの個数 *)
n_objects.(0) <- Array.make 1 0;

(* オブジェクトのデータを入れるベクトル（最大60個）*)
dummy.(0) <- Array.make 0 0.0;
let dummy0 = dummy.(0) in 
objects.(0) <- Array.make 60 (0, 0, 0, 0, dummy0, dummy0, false, dummy0, dummy0, dummy0, dummy0);

(* Screen の中心座標 *)
screen.(0) <- Array.make 3 0.0;
(* 視点の座標 *)
viewpoint.(0) <- Array.make 3 0.0;
(* 光源方向ベクトル (単位ベクトル) *)
light.(0) <- Array.make 3 0.0;
(* 鏡面ハイライト強度 (標準=255) *)
beam.(0) <- Array.make 1 255.0;
(* AND ネットワークを保持 *)
and_net.(0) <- Array.make 50 (Array.make 1 (-1));
(* OR ネットワークを保持 *)
or_net.(0) <- Array.make 1 (Array.make 1 (and_net.(0)));

(* 以下、交差判定ルーチンの返り値格納用 *)
(* solver の交点 の t の値 *)
solver_dist.(0) <- Array.make 1 0.0;
(* 交点の直方体表面での方向 *)
intsec_rectside.(0) <- Array.make 1 0;
(* 発見した交点の最小の t *)
tmin.(0) <- Array.make 1 (1000000000.0);
(* 交点の座標 *)
intersection_point.(0) <- Array.make 3 0.0;
(* 衝突したオブジェクト番号 *)
intersected_object_id.(0) <- Array.make 1 0;
(* 法線ベクトル *)
nvector.(0) <- Array.make 3 0.0;
(* 交点の色 *)
texture_color.(0) <- Array.make 3 0.0;

(* 計算中の間接受光強度を保持 *)
diffuse_ray.(0) <- Array.make 3 0.0;
(* スクリーン上の点の明るさ *)
rgb.(0) <- Array.make 3 0.0;

(* 画像サイズ *)
image_size.(0) <- Array.make 2 0;
(* 画像の中心 = 画像サイズの半分 *)
image_center.(0) <- Array.make 2 0;
(* 3次元上のピクセル間隔 *)
scan_pitch.(0) <- Array.make 1 0.0;

(* judge_intersectionに与える光線始点 *)
startp.(0) <- Array.make 3 0.0;
(* judge_intersection_fastに与える光線始点 *)
startp_fast.(0) <- Array.make 3 0.0;

(* 画面上のx,y,z軸の3次元空間上の方向 *)
screenx_dir.(0) <- Array.make 3 0.0;
screeny_dir.(0) <- Array.make 3 0.0;
screenz_dir.(0) <- Array.make 3 0.0;

(* 直接光追跡で使う光方向ベクトル *)
ptrace_dirvec.(0) <- Array.make 3 0.0;

(* 間接光サンプリングに使う方向ベクトル *)
dummyf.(0) <- Array.make 0 0.0;
dummyff.(0) <- Array.make 0 dummyf.(0);
dummy_vs.(0) <- Array.make 0 (dummyf.(0), dummyff.(0));
dirvecs.(0) <- Array.make 5 dummy_vs.(0);

(* 光源光の前処理済み方向ベクトル *)
dummyf2.(0) <- Array.make 0 0.0;
v3.(0) <- Array.make 3 0.0;
consts.(0) <- Array.make 60 dummyf2.(0);
light_dirvec.(0) <- (v3.(0), consts.(0));

(* 鏡平面の反射情報 *)
dummyf3.(0) <- Array.make 0 0.0;
dummyff3.(0) <- Array.make 0 dummyf3.(0);
dummydv.(0) <- (dummyf3.(0), dummyff3.(0));
reflections.(0) <- Array.make 180 (0, dummydv.(0), 0.0);

(* reflectionsの有効な要素数 *) 

n_reflections.(0) <- Array.make 1 0