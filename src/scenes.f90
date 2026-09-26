module scenes
  use raytracer, only:scene
  use primatives, only:plane_new, sphere
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
            [-40.0,-40.0,-20.0],& !point
            [0.2,0.2,1.0],& !normal

            [0, 0, 255, 255],& !color
            burningship&
          ),&

           plane_new(&
             [0.0, 200.0,-20.0],& !point
             [-0.2,-0.2,1.0],& !normal

             [237, 116, 253, 255], & !color
              checkerboard&
           ),&

           plane_new(&
             [60.0, 300.0,100.0],& !point
             [0.7,-0.4,1.0],& !normal

             [122,122, 255, 255],& !color
              mandlebrot&
           ),&

           plane_new(&
             [50.0, 250.0,500.0],& !point
             [-0.5,0.4,-1.0],& !normal

             [0,0, 255, 255],& !color
              powertower&
           )&
        ],&

        [& ! spheres
          sphere(&
            10,& ! radius
            [10.0, 10.0, 20.0],&

            [122,122, 255, 255],& !color
            burningship&
          ),&

           sphere(&
             100,& ! radius
             [0.0, 200.0, 300.0],&

             [122,122, 255, 255],& !color
              mandlebrot&
           ),&

           sphere(&
             100,& ! radius
             [0.0, 0.0, 200.0],&

             [122,122, 255, 255],& !color
             powertower&
           )&
        ]&
      )
  end function rgb_test
end module scenes
