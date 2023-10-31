import scala.io.StdIn

object CodeComplet {
  // Cette classe permet de créer un livre
  class Livre(val titre: String, val auteur: String, val annéeDePublication: Int, var estEmprunté: Boolean) {
    // Cette méthode permet d'indiquer le livre comme emprunté
    def emprunter(): Unit = {
      estEmprunté = true
    }

    // Cette méthode permet d'indiquer le livre comme emprunté
    def rendre(): Unit = {
      estEmprunté = false
    }
  }

  // Cette classe créer une bibliothèque comme étant une liste de livres
  class Bibliothèque {
    var livres: List[Livre] = List()

    // Cette méthode permet d'ajouter un livre à la bibliothèque
    def ajouterUnLivre(titre: String, auteur: String, année: Int): Unit = {
      val nouveauLivre = new Livre(titre, auteur, année, estEmprunté = false)
      livres = nouveauLivre :: livres
    }

    // Cette méthode permet d'emprunter un livre de la bibliothèque
    def emprunterUnLivre(titre: String): Option[Livre] = {
      val livreOption = livres.find(_.titre == titre)
      livreOption match {
        case Some(livre) =>
          if (!livre.estEmprunté) {
            livre.estEmprunté = true
            Some(livre)
          } else {
            println(s"Le livre \"$titre\" est déjà emprunté.")
            None
          }
        case None =>
          println(s"Le livre \"$titre\" n'existe pas dans la bibliothèque.")
          None
      }
    }

    // Cette méthode permet de rendre un livre à la bibliothèque
    def rendreUnLivre(titre: String): Option[Livre] = {
      val livreOption = livres.find(_.titre == titre)
      livreOption match {
        case Some(livre) =>
          if (livre.estEmprunté) {
            livre.estEmprunté = false
            Some(livre)
          } else {
            println(s"Le livre \"$titre\" n'est pas actuellement emprunté.")
            None
          }
        case None =>
          println(s"Le livre \"$titre\" n'existe pas dans la bibliothèque.")
          None
      }
    }

    // Cette méthode permet d'afficher la liste des livres (disponibles ou non)
    def afficherLaListeDeLivres(): Unit = {
      livres.foreach(livre => {
        println(s"Titre: ${livre.titre}, Auteur: ${livre.auteur}, Année de publication: ${livre.annéeDePublication}, Emprunté: ${if (livre.estEmprunté) "Oui" else "Non"}")
      })
    }
  }

  // Interaction avec l'utilisateur
  def main(args: Array[String]): Unit = {
    var continuer = true
    val bibliothèque = new Bibliothèque

    while (continuer) {
      println("\nQue voulez-vous faire dans cette biliothèque ?")
      println("1. Ajouter un livre")
      println("2. Emprunter un livre")
      println("3. Rendre un livre")
      println("4. Afficher la liste de livres")
      println("5. Quitter")
      print("Choix : ")

      val choix = StdIn.readInt()

      choix match {
        case 1 =>
          print("Titre du livre : ")
          val titre = StdIn.readLine()
          print("Auteur : ")
          val auteur = StdIn.readLine()
          try {
            print("Année de publication : ")
            val annee = StdIn.readInt()
            bibliothèque.ajouterUnLivre(titre, auteur, annee)
          } catch {
            case e: NumberFormatException =>
              println("L'année de publication que vous avez indiqué est invalide.")
          }

        case 2 =>
          print("Titre du livre à emprunter : ")
          val titre = StdIn.readLine()
          bibliothèque.emprunterUnLivre(titre)

        case 3 =>
          print("Titre du livre à rendre : ")
          val titre = StdIn.readLine()
          bibliothèque.rendreUnLivre(titre)

        case 4 =>
          println("\nListe des livres dans la bibliothèque:")
          bibliothèque.afficherLaListeDeLivres()

        case 5 =>
          continuer = false

        case _ =>
          println("Ce numéro n'est pas valide, veuillez choisir une option valide.")
      }
    }
  }
}
