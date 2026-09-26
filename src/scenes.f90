module scenes
  use raytracer, only:scene
  use primatives, only:plane_new, sphere_new
  use shaders

  implicit none(type, external)
  public
  contains
  function rgb_test() result(ret)
    type(scene) :: ret
      ret = scene(&
        [0.0,0.0,0.0], [1.0,1.0,1.0],&
        [229, 221, 107, 255],&
        [&!planes
          plane_new(&
            [-627.0,87.0,23.0],& !point
            [0.2,0.2,1.0],& !normal

            [0, 0, 128, 255],& !color
            burningship&
          ),&

           plane_new(&
             [0.0, 200.0,-20.0],& !point
             [-0.2,-0.2,1.0],& !normal

             [237, 116, 253, 255], & !color
              checkerboard&
           ),&

           plane_new(&
             [-279.0,-254.0,319.0],& !point
             [0.5, -0.1,0.1],& !normalt

             [79 , 103, 0, 255],& !color
              mandlebrot&
           ),&

           plane_new(&
             [-772.0, 338.0,955.0],& !point
             [-0.5,0.4,-1.0],& !normal

             [79 , 103, 0, 255],& !color
              powertower&
           )&
        ],&

        [& ! spheres
          sphere_new(&
            60.0,& ! radius
            [157.0, -103.0, 137.0],&

            [6,6, 120, 255],& !color
            checkerboard&
          ),&

           sphere_new(&
             100.0,& ! radius
             [0.0, 200.0, 300.0],&

             [122,122, 255, 255],& !color
              mandlebrot&
           ),&

           sphere_new(&
             150.0,& ! radius
             [317.0, 574.0, 173.0],&

             [122,122, 255, 255],& !color
              burningship&
            )&
        ]&
      )
  end function rgb_test
end module scenes
