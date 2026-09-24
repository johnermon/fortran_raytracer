module scenes
  use raytracer, only:scene
  use primatives, only:plane, sphere
  use shaders

  implicit none(type, external)
  public
  contains
  pure function rgb_test() result(ret)
    type(scene) :: ret
      ret = scene(&
        [0.0,0.0,0.0], [1.0,1.0,1.0],&
        [229, 221, 107, 255],&
        [&!planes
          plane(&
            [0.0,0.0,-20.0],& !point
            [0.2,0.2,1.0],& !normal

            [0, 0, 255, 255],& !color
            rainbow&
          ),&

           plane(&
             [0.0, 200.0,-20.0],& !point
             [-0.2,-0.2,1.0],& !normal

             [0, 255, 0, 255], & !color
              0&
           ),&

           plane(&
             [0.0, 200.0,-20.0],& !point
             [0.5,-0.4,1.0],& !normal

             [255, 0, 0, 255],& !color
              0&
           )&
        ],&

        [& ! spheres
          sphere(&
            10,& ! radius
            [0.0, 0.0, 0.0],&

            [122,122, 255, 255]& !color
          )&
        ]&
      )
  end function rgb_test
end module scenes
