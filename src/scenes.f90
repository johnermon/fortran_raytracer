module scenes
  use raytracer, only:scene_new, scene
  use primatives, only:plane_new, sphere_new
  use shaders

  implicit none(type, external)
  public
  contains
  function rgb_test() result(ret)
    use, intrinsic::iso_fortran_env, only:uint8
    type(scene) :: ret
      ret = scene_new(&
        [0.0,0.0,0.0], [1.0,1.0,1.0],&
        uint([229, 221, 107, 255], kind=uint8),&
        [&!planes
          plane_new(&
            [-627.0,87.0,23.0],& !point
            [0.2,0.2,1.0],& !normal

            uint([0, 0, 255, 255], kind=uint8),& !color
            burningship&
          ),&

           plane_new(&
             [0.0, 200.0,-20.0],& !point
             [-0.2,-0.2,1.0],& !normal

             uint([237, 116, 253, 255],kind=uint8), & !color
              checkerboard&
           ),&

           plane_new(&
             [-279.0,-254.0,319.0],& !point
             [0.5, -0.1,0.1],& !normalt

             uint([79, 103, 0, 255],kind=uint8),& !color
              mandlebrot&
           ),&

           plane_new(&
             [-772.0, 338.0,955.0],& !point
             [-0.5,0.4,-1.0],& !normal

             uint([79, 103, 0, 255],kind=uint8),& !color
              powertower&
           )&
        ],&

        [& ! spheres
          sphere_new(&
            60.0,& ! radius
            [157.0, -103.0, 137.0],&

            uint([6,6, 120, 255],kind=uint8),& !color
            checkerboard&
          ),&

           sphere_new(&
             100.0,& ! radius
             [0.0, 200.0, 300.0],&

             uint([122,122, 255, 255],kind=uint8),& !color
              mandlebrot&
           ),&

           sphere_new(&
             150.0,& ! radius
             [317.0, 574.0, 173.0],&

             uint([122,122, 255, 255],kind=uint8),& !color
              burningship&
            )&
        ]&
      )
  end function rgb_test
end module scenes
