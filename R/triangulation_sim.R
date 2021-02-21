
#' triangulation_sim
#'
#' @param size
#' @param center
#' @param deviation
#' @param LeftPosition
#' @param RightPosition
#' @param ElectionGravity
#' @param LeftWinGravity
#' @param RightWinGravity
#' @param randomness
#' @param Shift
#' @param TriangulationShift
#' @param elections
#' @param trials
#'
#' @return
#' @export
#'
#' @examples


triangulation_sim <-
   function(
      size = 5000,
      center = 0,
      deviation = 3,
      LeftPosition = 1,
      RightPosition = -2,
      ElectionGravity = .1,
      LeftWinGravity = 0.05,
      RightWinGravity = 0.05,
      randomness = 0.1,
      Shift = 0,
      TriangulationShift = 0.11,
      elections = 20,
      trials = 1
   ) {

      # Initialize vectors

      Lwin <- vector("numeric")
      Lwin2 <- vector("numeric")
      Rwin <- vector("numeric")
      Rwin2 <- vector("numeric")
      meanideology <- vector("numeric")
      Xaxis <- vector("numeric")
      LeftPositionList <- vector("numeric")
      RightPositionList <- vector("numeric")

      # Prepare progessor

      p <- progressr::progressor(steps = trials*elections)

      # Start looping

      for (i in seq(from = 1, to = trials)) {

         LeftPositionTemp <- LeftPosition
         RightPositionTemp <- RightPosition

         pop <- rnorm(size, center, deviation)

         for (cycle in seq(from = 1, to = elections)) {

            #triangulation (shifts party/ies to center)
            if (LeftPositionTemp > sum(pop)/length(pop)) {
               LeftPositionTemp = LeftPositionTemp - TriangulationShift
            } else if (LeftPositionTemp < sum(pop)/length(pop)) {
               LeftPositionTemp = LeftPositionTemp + TriangulationShift
            }

            #shift calc (natural ideological shift)
            pop <-
               purrr::map_dbl(
                  pop,
                  ~.x + Shift*cycle
               )

            #election gravity calc (population ideology shifts according to campaigns)
            pop <-
               purrr::map_dbl(
                  pop,
                  ~.x + (ElectionGravity * LeftPositionTemp) + (ElectionGravity * RightPositionTemp)
               )

            #election
            #Rewritten extensively by DSB

            rvotes = 0

            left <-
               purrr::map_dbl(
                  pop,
                  ~abs(.x - LeftPositionTemp) + randomness * runif(1)
               )

            right <-
               purrr::map_dbl(
                  pop,
                  ~abs(.x - RightPositionTemp) + randomness * runif(1)
               )

            rvotes <-
               sum(right > left)

            #election2

            if (rvotes > length(pop)/2) {
               pop <-
                  purrr::map_dbl(
                     pop,
                     ~.x + (RightWinGravity * RightPositionTemp)
                  )

               #plot stuff
               Rwin <- append(Rwin, 100)
               Rwin2 <- append(Rwin2, -100)
               Lwin <- append(Lwin, 0)
               Lwin2 <- append(Lwin2, 0)

            } else {

               pop <-
                  purrr::map_dbl(
                     pop,
                     ~.x + (LeftWinGravity * LeftPositionTemp)
                  )

               #plot stuff
               Rwin <- append(Rwin, 0)
               Rwin2 <- append(Rwin2, 0)
               Lwin <- append(Lwin, 100)
               Lwin2 <- append(Lwin2, -100)

            }

            # some final parameters for plotting

            meanideology <-
               append(
                  meanideology,
                  sum(pop)/length(pop)
               )

            Xaxis <-
               c(Xaxis, cycle)

            LeftPositionList <-
               append(
                  LeftPositionList,
                  LeftPositionTemp
               )

            RightPositionList <-
               append(
                  RightPositionList,
                  RightPositionTemp
               )


            p(
               1, message = paste0("Simulating voter ideology...")
            )

         }

      }

      # Construct dataframe for ggplot

      sim_data <-
         tibble::tibble(
            Xaxis = Xaxis,
            Lwin = Lwin,
            Lwin2 = Lwin2,
            Rwin = Rwin,
            Rwin2 = Rwin2,
            meanideology = meanideology,
            LeftPositionList = LeftPositionList,
            RightPositionList = RightPositionList
         )

      # Construct plot

      ggplot2::ggplot(sim_data) +
         ggplot2::geom_point(
            ggplot2::aes(Xaxis, meanideology),
            color = "black",
            size = 1.5
         ) +
         ggplot2::geom_line(
            ggplot2::aes(Xaxis, LeftPositionList),
            color = "blue",
            size = 1.5,
            linetype = 2
         ) +
         ggplot2::geom_line(
            ggplot2::aes(Xaxis, RightPositionList),
            color = "red",
            size = 1.5,
            linetype = 2
         ) +
         ggplot2::scale_size_identity() +
         ggplot2::lims(
            x = c(1, elections),
            y = c(-5, 5)
         ) +
         ggplot2::labs(
            x = "Number of Election Cycles",
            y = "Average Voter Ideology"
         ) +
         ggplot2::theme_minimal() +
         ggplot2::theme(
            text = ggplot2::element_text(size = 16)
         )

   }




