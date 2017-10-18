(ns figura.examples
  (:require [figura.viewer        :refer [make-viewer set-model! load-mesh save-mesh]]
            [thi.ng.geom.aabb     :refer [aabb]]
            [thi.ng.geom.bezier   :as b]
            [thi.ng.geom.circle   :refer [circle]]
            [thi.ng.geom.core     :as g]
            [thi.ng.geom.mesh.csg :as csg]
            [thi.ng.geom.cuboid   :refer [cuboid]]
            [thi.ng.geom.gl.core  :as gl]
            [thi.ng.geom.gmesh    :as gm]
            [thi.ng.geom.matrix   :as mat]
            [thi.ng.geom.ptf      :as ptf]
            [thi.ng.geom.rect     :refer [rect]]
            [thi.ng.geom.sphere   :refer [sphere]]
            [thi.ng.geom.utils    :as gu]
            [thi.ng.geom.vector   :as v :refer [vec2 vec3 V3Y]]
            [thi.ng.math.core     :as m]))

;; https://ello.co/jackrusher

(make-viewer)

(set-model!
 (load-mesh "/Users/jack/src/quil-sketches/data/duck.stl" (aabb 2)))

(set-model!
 (load-mesh "/Users/jack/src/quil-sketches/homer-just-head.stl" (aabb 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WALL PANEL

(def h 40)
(def w 40)

(def height-map
  (let [heights (make-array java.lang.Double (* (inc h) (inc w)))]
    (amap heights idx ret (aset heights idx (m/random 1 3.5)))))

(defn heights->mesh [heights w h]
  (let [get-height (fn [h-map x y] (aget h-map (+ (* y w) x)))]
    (reduce
     #(g/into %1 (g/tessellate (g/as-mesh %2)))
     (gm/gmesh)
     (for [y1 (range w)
           x1 (range h)
           :let [x2 (inc x1)
                 y2 (inc y1)]]
       (cuboid [x1 y2 1]
               [x1 y2 (get-height height-map x1 y2)]
               [x2 y2 (get-height height-map x2 y2)]
               [x2 y2 1]
               [x1 y1 1]
               [x1 y1 (get-height height-map x1 y1)]
               [x2 y1 (get-height height-map x2 y1)]
               [x2 y1 1])))))

(set-model!
 (-> (heights->mesh height-map w h) g/center))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PTF

(defn cinquefoil
  [t]
  (let [t  (* t m/TWO_PI)
        pt (* 2.0 t)
        qt (* 5.0 t)
        qc (+ 3.0 (Math/cos qt))]
    (v/vec3 (* qc (Math/cos pt)) (* qc (Math/sin pt)) (Math/sin qt))))

(-> (ptf/sweep-mesh
     (mapv cinquefoil (m/norm-range 400))     
     (g/vertices (circle 0.75) 5)
     {:align? true :loop? true})
    set-model!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STRANGELY ATTRACTIVE

(defn lorenz-points [p sigma beta dt]
  (loop [n   5000
         x   0.01
         y   0
         z   0
         out []]
    (if (= 0 n)
      out
      (recur (dec n)
             (+ x (* p (- y x) dt))
             (+ y (* (- (* x (- sigma z)) y) dt))
             (+ z (* (- (* x y) (* beta z)) dt))
             (conj out (vec3 x y z))))))

(set-model!
 (ptf/sweep-mesh (lorenz-points 11 28 (/ 8.0 3.0) 0.005)
                 (g/vertices (circle 0.5) 22)
                 {:align? true :loop? false}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CSG

(set-model!
 (-> (aabb 50 50 50)
     g/center
     g/as-mesh
     g/tessellate))

(let [n-boxes 6]
  (-> (reduce csg/subtract
              (-> (aabb 50)
                  g/center
                  g/as-mesh
                  g/tessellate
                  csg/mesh->csg)
              (map #(-> (aabb 50)
                        g/center
                        (g/translate [-50 0 0])
                        (g/rotate (m/radians (* % (/ 360 n-boxes))))
                        g/as-mesh
                        g/tessellate
                        csg/mesh->csg)
                   (range n-boxes)))
;;      (csg/intersect (-> (sphere 32) g/center g/as-mesh g/tessellate csg/mesh->csg))
      csg/csg->mesh
      set-model!))

(def duck-csg
  (csg/mesh->csg 
   (load-mesh "/Users/jack/src/quil-sketches/data/duck.stl" (aabb 2))))

(set-model! (csg/csg->mesh duck-csg))

(set-model! (csg/csg->mesh 
             (csg/intersect duck-csg
                            (-> (aabb 2 1 2)
                                (g/translate [-0.4 0 -0.55])
                                g/as-mesh
                                g/tessellate
                                csg/mesh->csg))))


